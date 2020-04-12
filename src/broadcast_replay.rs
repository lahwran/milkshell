use crate::multiplexer::SingleHalf;
use futures::future::join_all;
use futures::{SinkExt, StreamExt};
use serde::Deserialize;
use serde_json;
use serde_json::json;
use std::mem::swap;
use tokio::{select, sync::mpsc};

#[derive(Deserialize, Debug)]
struct MultiplexedMessage {
    stream_id: String,
    message: serde_json::Value,
}
enum ShouldStop {
    Stop,
    Continue,
}
//
//pub(crate) struct DropGuard<F: FnOnce() -> ()> {
//    drop_callback: Option<F>,
//}
//
//impl<F: FnOnce() -> ()> DropGuard<F> {
//    fn new(f: F) -> DropGuard<F> {
//        DropGuard {
//            drop_callback: Some(f),
//        }
//    }
//}
//
//impl<F: FnOnce() -> ()> Drop for DropGuard<F> {
//    fn drop(&mut self) {
//        let a = &mut self.drop_callback;
//        let mut b = Option::None;
//        swap(a, &mut b);
//        let c = b.expect("someone removed my callback. no do dat.");
//        c();
//    }
//}

pub(crate) struct BroadcastConnector {
    in_sink: Option<mpsc::Sender<serde_json::Value>>,
    outsink_sink: Option<mpsc::Sender<mpsc::Sender<serde_json::Value>>>,
}

impl BroadcastConnector {
    // to be called by network side. returns (in_sink, out_source): in_sink to send messages coming in from the outside world, out_source to receive messages that should go to the outside world
    pub(crate) async fn connect(
        &mut self,
    ) -> (
        mpsc::Sender<serde_json::Value>,
        mpsc::Receiver<serde_json::Value>,
    ) {
        let (out_sink, out_source) = mpsc::channel(10);
        self.outsink_sink
            .as_mut()
            .unwrap()
            .send(out_sink)
            .await
            .expect("Subscribing to write messages failed");
        (self.in_sink.as_mut().unwrap().clone(), out_source)
    }
}

pub(crate) fn launch_broadcasters(single: SingleHalf<serde_json::Value>) -> BroadcastConnector {
    let SingleHalf {
        mut subscription_receiver,
        mut subscriptions,
        mut receiver,
    } = single;
    let (outsink_sink, mut outsink_source) = mpsc::channel(10);
    let (in_sink, mut in_source) = mpsc::channel(10);

    tokio::spawn(async move {
        loop {
            let should_stop = select! {
                maybe_subscription = subscription_receiver.recv() => {
                    match maybe_subscription {
                        Some((name, maybe_sub)) => match maybe_sub {
                            Some(sub) => { subscriptions.insert(name, sub); ShouldStop::Continue },
                            None => { subscriptions.remove(&name); ShouldStop::Continue }
                        },
                        None => {
                            println!("multihalf's subscription_sender was dropped, need websocket to shut down cleanly");
                            ShouldStop::Stop
                        }
                    }
                },
                maybe_message = in_source.recv() => {
                    let maybe_message = serde_json::from_value(maybe_message.expect("in_sink was somehow dropped"));
                    let message: MultiplexedMessage = maybe_message.expect("FIXME: Got invalid MultiplexedMessage from client"); // TODO: needs better error handling
                    let subscriber = subscriptions.get_mut(&message.stream_id).expect("FIXME: Got message for subscriber that wasn't listening, this should be handled"); // TODO FIXME: this error can happen during normal functioning
                    // TODO: a sufficiently backed up stream can hang the multiplexer with backpressure. is this acceptable?
                    subscriber.send(message.message).await.expect("FIXME: sent message to subscriber that stopped listening while we were trying to send it");
                    ShouldStop::Continue
                }
            };
            match should_stop {
                ShouldStop::Stop => {
                    break;
                }
                ShouldStop::Continue => {}
            };
        }
    });

    // TODO: notify this task proactively when an unsubscription happens, rather than waiting for
    //  it to handle errors writing to a stream?
    tokio::spawn(async move {
        let mut subs = Vec::new();
        let mut last_message: Option<serde_json::Value> = None;
        loop {
            select! {
                maybe_subscription = outsink_source.recv() => {
                    let mut subscription: mpsc::Sender<serde_json::Value> = match maybe_subscription {
                        Some(x) => x,
                        None => {println!("subscription source ended, shutting down"); return}
                    };
                    if let Some(msg) = last_message.as_ref() {
                        match subscription.send(msg.clone()).await {
                            Ok(_) => (),
                            Err(_) => continue
                        };
                    }
                    subs.push(Some(subscription));
                },
                maybe_message = receiver.recv() => {
                    let (stream_id, message) = match maybe_message {
                        Some(m) => m,
                        None => {
                            println!("Multiplexer sender was dropped! no more messages.");
                            return
                        } // multiplexer sender was dropped
                    };
                    let message_wrapped = json!({
                        "stream_id": stream_id,
                        "message": message
                    });
                    last_message = Some(message_wrapped.clone());
                    let result = {
                        let futures = subs.iter_mut().enumerate().filter_map(|(idx, x): (usize, &mut Option<mpsc::Sender<serde_json::Value>>)| match x {
                            Some(val) => Some((idx, val.send(message_wrapped.clone()))),
                            None => None
                        }).map(|(idx, future)| async move { tokio::join!(futures::future::ok::<usize, usize>(idx), future) });
                        // this bottlenecks all clients on the slowest client, after buffering. how much buffer should there be in send-to-client?
                        // lauren you're overthinking this it's an editor client
                        join_all(futures).await
                    };
                    for (idx, val) in result.iter() {
                        let idx = idx.unwrap();
                        if !val.is_ok() {
                            println!(" -> Error sending message to stream, assuming it's gone.\n    error: {:?}\n    idx: {:?} streams: {:?}", val, idx, subs);
                            subs[idx] = None;
                        }
                    }
                }
            }
        }
    });
    BroadcastConnector {
        in_sink: Some(in_sink),
        outsink_sink: Some(outsink_sink),
    }
}

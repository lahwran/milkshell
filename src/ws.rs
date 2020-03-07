use serde::Deserialize;

use std::error::Error;
use tokio::net::{TcpListener, TcpStream};
use tokio::sync::mpsc;

// have to use this particular split function on tcp sockets - tcpstream.split() doesn't work

use futures::{SinkExt, StreamExt};

use crate::multiplexer::SingleHalf;
use futures::future::join_all;
use serde_json::json;
use tokio::select;

async fn accept_connection(
    stream: TcpStream,
    mut receiver: mpsc::Receiver<::serde_json::Value>,
    mut sender: mpsc::Sender<::serde_json::Value>,
) {
    let addr = stream
        .peer_addr()
        // TODO: when is this error possible?
        .expect("(err 0) connected streams should have a peer address");
    println!("Peer address: {}", addr);

    let ws_stream = tokio_tungstenite::accept_async(stream)
        .await
        // TODO ERROR: handle errors better here
        .expect("(err 1) Error during the websocket handshake");

    println!("New WebSocket connection: {}", addr);

    // backwards from the tokio one because woo hoo yay
    let (mut writer, mut reader) = ws_stream.split();
    //let mut rx = tx.subscribe();

    tokio::spawn(async move {
        // In a loop, read data from the socket and write the data back.
        loop {
            let read_result = reader.next().await;
            // TODO ERROR: what errors are possible here?
            let tungstenite_result = read_result.expect("(err 3) failed to read data from socket");
            let msg = match tungstenite_result {
                Ok(m) => m,
                Err(e) => {
                    println!("Error parsing incoming message with tungstenite, quitting read loop. error: {:?}", e);
                    return;
                }
            };
            // TODO: this could pass around fixed-size buffers with slice lengths, is that faster?
            let parsed_message = match msg {
                tungstenite::Message::Text(string) => {
                    serde_json::from_str(&string).expect("received invalid json")
                    //sender.send().await.expect("wat, multiplexer task dropped receiver?"); // TODO: display invalid message on error
                    //println!("received: {:?}", string);
                    //tx.send(string.into_bytes()).unwrap();
                }
                tungstenite::Message::Binary(data) => {
                    serde_json::from_slice(&data).expect("received invalid json")
                    //println!("received: {:?}", data);
                    //tx.send(data).unwrap();
                }
                tungstenite::Message::Close(data) => {
                    println!(
                        "Got close message, will exit read loop. Close message data: {:?}",
                        data
                    );
                    break;
                }
                // TODO ERROR: handle unknown message types
                other => panic!(format!("Unknown message type: {:?}", other)),
            };
            sender
                .send(parsed_message)
                .await
                .expect("incoming message receiver was somehow dropped by multiplexer, wat");
        }
    });

    tokio::spawn(async move {
        // In a loop, read data from the socket and write the data back.
        loop {
            //let value = rx.recv().await.unwrap();
            let message = match receiver.recv().await {
                Some(pair) => pair,
                None => return,
            };
            println!("receive and send message: {:?}", message);
            let result = writer
                .send(tungstenite::Message::Text(message.to_string()))
                .await;
            match result {
                Ok(_) => (),
                Err(e) => {
                    println!("Error writing to socket, dropping write task: {:?}", e);
                    break;
                }
            }
            // TODO ERROR: handle ConnectionClosed
            //.expect("failed to write data to socket");
        }
    });
}

#[derive(Deserialize, Debug)]
struct MultiplexedMessage {
    stream_id: String,
    message: serde_json::Value,
}

enum ShouldStop {
    Stop,
    Continue,
}

pub(crate) async fn run_websocket(
    single: SingleHalf<serde_json::Value>,
) -> Result<(), Box<dyn Error>> {
    let addr = "127.0.0.1:13579".to_string();

    let SingleHalf {
        mut subscription_receiver,
        mut subscriptions,
        mut receiver,
    } = single;
    let (mut subchan_send, mut subchan_recv) = mpsc::channel(10);
    let (recvchan_send, mut recvchan_recv) = mpsc::channel(10);

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
                maybe_message = recvchan_recv.recv() => {
                    let maybe_message = serde_json::from_value(maybe_message.expect("recvchan_send was somehow dropped"));
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
        loop {
            select! {
                maybe_subscription = subchan_recv.recv() => {
                    subs.push(Some(maybe_subscription.expect("subchan_send was somehow dropped")));
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

    // Next up we create a TCP listener which will listen for incoming
    // connections. This TCP listener is bound to the address we determined
    // above and must be associated with an event loop.
    let mut listener = TcpListener::bind(&addr).await?;
    println!("Listening on: {}", addr);
    tokio::spawn(async move {});

    //let (tx, mut rx1) = broadcast::channel(16);

    loop {
        let (socket, _) = listener.accept().await?;
        let (local_send, local_recv) = mpsc::channel(10);
        subchan_send
            .send(local_send)
            .await
            .expect("Subscribing to write messages failed");
        accept_connection(socket, local_recv, recvchan_send.clone()).await;
    }
}

//fn handle_client(stream: TcpStream) {}

//pub fn net_test_main() {}

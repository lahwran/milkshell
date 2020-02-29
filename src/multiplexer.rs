use async_std::sync::{channel, Receiver, Sender};
use std::collections::HashMap;

pub(crate) struct SingleHalf<T> {
    subscription_receiver: Receiver<(String, Option<Sender<T>>)>,
    subscriptions: HashMap<String, Sender<T>>,
    // TODO: buffers for messages received with nowhere to send them yet? or should we attempt a guarantee that that can't happen?
    //  if getting a message for a stream that doesn't exist yet is valid at all, need to send backpressure messages
    receiver: Receiver<(String, T)>,
}

pub(crate) struct MultiHalf<T> {
    sender: Sender<(String, T)>,
    subscription_sender: Sender<(String, Option<Sender<T>>)>,
}

pub(crate) fn multiplexer<T>(
    send_buffer: usize,
    subscription_buffer: usize,
) -> (SingleHalf<T>, MultiHalf<T>) {
    let (sender, receiver) = channel(send_buffer);
    let (subscription_sender, subscription_receiver) = channel(subscription_buffer);
    return (
        SingleHalf {
            receiver,
            subscription_receiver,
            subscriptions: HashMap::new(),
        },
        MultiHalf {
            sender,
            subscription_sender,
        },
    );
}

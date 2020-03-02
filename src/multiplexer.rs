use async_std::sync::{channel, Receiver, Sender};
use futures::prelude::*;
use futures::task::{Context, Poll};
use futures::{Sink, SinkExt, Stream, StreamExt};
use std::collections::HashMap;
use std::error::Error;
use std::mem;

pub(crate) struct SingleHalf<T> {
    pub subscription_receiver: Receiver<(String, Option<Sender<T>>)>,
    pub subscriptions: HashMap<String, Sender<T>>,
    // TODO: buffers for messages received with nowhere to send them yet? or should we attempt a guarantee that that can't happen?
    //  if getting a message for a stream that doesn't exist yet is valid at all, need to send backpressure messages
    pub receiver: Receiver<(String, T)>,
}

#[derive(Clone)]
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

pub(crate) struct SubstreamWrite<T> {
    name: String,
    sender: Sender<(String, T)>,
}
pub(crate) struct SubstreamRead {}

impl<T> SubstreamWrite<T> {
    pub(crate) async fn send(&self, message: T) {
        self.sender.send((self.name.clone(), message)).await
    }
}

impl<T> MultiHalf<T> {
    pub(crate) fn get_stream(
        &self,
        stream: &str,
    ) -> Result<(SubstreamWrite<T>, SubstreamRead), String> {
        let a = stream.to_string();
        Ok((
            SubstreamWrite {
                name: stream.to_string(),
                sender: self.sender.clone(),
            },
            SubstreamRead {},
        ))
    }
}

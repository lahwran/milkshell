use crossbeam::thread;
use std::error::Error;
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::net::{TcpListener, TcpStream};
use tokio::sync::mpsc;

use std::{env, time};
// have to use this particular split function on tcp sockets - tcpstream.split() doesn't work
use tokio::io::split;

use futures::{join, SinkExt, Stream, StreamExt};
use std::time::{Duration, Instant};
use tokio::runtime::Runtime;

use crate::multiplexer::SingleHalf;
#[macro_use]
use futures::future::join_all;
use futures::prelude::*;
use serde_json::json;
use std::collections::HashSet;
use tokio::select;

async fn accept_connection(stream: TcpStream, mut receiver: mpsc::Receiver<::serde_json::Value>) {
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
            let msg = reader
                .next()
                .await
                // TODO ERROR: what errors are possible here?
                .expect("(err 3) failed to read data from socket")
                .expect("(err 4) tungstenite error");
            // TODO: this could pass around fixed-size buffers with slice lengths, is that faster?
            match msg {
                tungstenite::Message::Text(string) => {
                    println!("received: {:?}", string);
                    //tx.send(string.into_bytes()).unwrap();
                }
                tungstenite::Message::Binary(data) => {
                    println!("received: {:?}", data);
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
            }
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

pub(crate) async fn run_websocket(
    single: SingleHalf<serde_json::Value>,
) -> Result<(), Box<dyn Error>> {
    let addr = "127.0.0.1:13579".to_string();

    let SingleHalf {
        subscription_receiver,
        subscriptions,
        receiver,
    } = single;
    let (mut subchan_send, subchan_recv) = mpsc::channel(10);

    tokio::spawn(async move {
        let mut subs = Vec::new();
        loop {
            select! {
                maybe_subscription = subchan_recv.recv() => {
                    subs.push(maybe_subscription.expect("subchan_send was somehow dropped"));
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
                    let futures = subs.iter().map(|sub: &mpsc::Sender<serde_json::Value>| sub.send(message_wrapped.clone()));
                    let results = join_all!(futures).await;
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
        subchan_send.send(local_send).await;
        accept_connection(socket, local_recv).await;
    }
}

fn handle_client(stream: TcpStream) {}

pub fn net_test_main() {}

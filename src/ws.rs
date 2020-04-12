use std::error::Error;
use tokio::net::{TcpListener, TcpStream};
use tokio::sync::mpsc;

// have to use this particular split function on tcp sockets - tcpstream.split() doesn't work

use futures::{SinkExt, StreamExt};

use crate::broadcast_replay;
use crate::multiplexer::SingleHalf;

async fn accept_connection(
    stream: TcpStream,
    mut in_sink: mpsc::Sender<::serde_json::Value>,
    mut out_source: mpsc::Receiver<::serde_json::Value>,
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
            in_sink
                .send(parsed_message)
                .await
                .expect("incoming message receiver was somehow dropped by multiplexer, wat");
        }
    });

    tokio::spawn(async move {
        // In a loop, read data from the socket and write the data back.
        loop {
            //let value = rx.recv().await.unwrap();
            let message = match out_source.recv().await {
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

    let mut connector = broadcast_replay::launch_broadcasters(single);

    // Next up we create a TCP listener which will listen for incoming
    // connections. This TCP listener is bound to the address we determined
    // above and must be associated with an event loop.
    let mut listener = TcpListener::bind(&addr).await?;
    println!("Listening on: {}", addr);
    tokio::spawn(async move {});

    //let (tx, mut rx1) = broadcast::channel(16);

    loop {
        let (socket, _) = listener.accept().await?;
        let (out_source, in_sink) = connector.connect().await;
        accept_connection(socket, out_source, in_sink).await;
    }
}

//fn handle_client(stream: TcpStream) {}

//pub fn net_test_main() {}

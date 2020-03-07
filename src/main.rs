#![allow(dead_code)]
//#![feature(type_name_of_val)]
#![deny(unused_must_use)]
#![allow(unused_imports)]

extern crate pest;
#[macro_use]
extern crate pest_derive;

use serde::Deserialize;
use std::fmt::Write;

use futures::StreamExt;
use futures::{Future, Stream};
use serde_json::json;
use std::error::Error;

use crate::launch::side_effect_run_pipeline;
use crate::parse::Env::Python;
use crate::parse::{compile, parse};
use async_std::pin::Pin;
use futures::future;
use futures::task::{Context, Poll};
use std::time::{Duration, Instant};

use crate::stream_scan::ReducerExt;

mod codegen;
mod dir_scan;
mod launch;
mod multiplexer;
mod parse;
mod stream_scan;
mod ws;
//mod stream_scan;
//use stream_scan::*;

/*
from milkshell import receiver, output, check_pipeline

def lb1(input):
    <reindented input>

def lb2(input):
    <reindented input>

async def main():
    pipeline = input()

    pipeline = lb1(pipeline)
    check_pipeline(pipeline)

    pipeline = lb2(pipeline)
    check_pipeline(pipeline)

    await output(pipeline)

trio.run(main)
*/

// addr is an enum specifying how to connect
// ideally could choose per process whether to connect as stream or as sink, where streams
// launch_pipeline(Vec<f(input_source_addr, output_sink_addr, debug_socket_addr) -> supervision_handle>)

async fn asyncmain() {
    let res = compile(
        parse(
            r##"
                map @py {
                    v + 1
                } | @py { import derp; print("hello") } around the world
            "##,
        )
        .unwrap(),
        Python,
    )
    .unwrap();
    for pipeline in &res {
        side_effect_run_pipeline(pipeline).await;
    }
    println!("compile result: {:?}", &res)
}

#[derive(Copy, Clone)]
struct CoreState {}

#[derive(Deserialize)]
#[serde(tag = "type")]
enum CoreAction {
    Disconnect,
}

async fn run_core(mut m: multiplexer::MultiHalf<serde_json::Value>) {
    println!("launch core");
    let (mut _writer, reader) = m.get_stream("commandlist", 10).await.unwrap();
    let _reader_task = reader
        .reducer(CoreState {}, |state, maybe_action| {
            println!("Action: {:?}", maybe_action);
            let action = match maybe_action {
                None => {
                    println!("Received end-of-stream from client");
                    return CoreState {};
                }

                Some(serde_json::Value::Object(x)) => x,
                Some(other) => panic!(format!("Received non-object: {:?}", other)),
            };
            match action
                .get("type")
                .expect("Missing action type")
                .as_str()
                .expect("action type isn't string")
            {
                "action" => CoreState {},
                other => {
                    println!("unhandled message type: {:?}", other);
                    state
                }
            }
        })
        .skip_while(|_| future::ready(true))
        .next()
        .await;
}

#[tokio::main]
async fn main() {
    let (single, multi) = multiplexer::multiplexer(1, 1);

    tokio::spawn(async move {
        run_core(multi).await;
    });
    ws::run_websocket(single).await.unwrap();

    tokio::time::delay_for(Duration::from_secs(1)).await;
    // multiplexer receives messages and sends them on to the appropriate channel
    // multiplexer has two parts:
    // 1. the sender, which goes to the websocket and contains the appropriate stream pair,
    //      as well as one additional stream: a Source<Tuple<String, Option<Sink<message>>>>. this will receive messages to associate streams with their io.
    //      messages received over the websocket connection will be sent to the appropriate place, queueing them if necessary. (TODO: backpressure something or other)
    // 2. the receiver, which goes to the core and contains the other stream pair, and which can return substream io pairs by name. when you do that,
    //      it sends a (name, Some(Sink)) message with the write sink to the new streampair over the queue to the sender, and then it returns a struct that contains a copy
    //      of the main sink for the websocket and the name of the stream. when sending messages, it wraps them into the name and the message, and sends them on.
    //      (TODO: it may need a .close() method to get order of closes and drains right - you can't send a disconnect request on drop of the rx side, and there's a race condition with sending a drop just after a message sent to the tx side)
    //      the receiver keeps track of which streams it's handed out and gets cranky if one is asked for while it's in use.
    // the startup process:
    // 1. create the multiplexer. this will require two channels set up as two stream pairs, and
    //      a channel for the stream stream. it will produce a tuple of two structs.
    // 2. pass the parts to the two ends - the core task and the websocket task.
    // 3. in the core task, pull out the command list stream from the multiplexer. it allows appending and removing commands, and getting the count (?) of current commands.
    //      it only acts on message from the client, so simply iterate client incoming messages, update state, and send replies. maintain an array of the references for command handlers,
    //      and launch/shut down the command handlers when they're appended or deleted (or frozen?)
    // 4. launch command handlers for each slot, with a state update loop like a reducer, sending back the full state
    //      of the command every time anything happens. handle at least {set text: value}, which updates a parse in state, and {run}, which launches the processes with new stream ids, passing each stream id into each process launch,
    //      and updates the state with their stream ids. might not need anything else for now.
    //
    //asyncmain().await
}

#[cfg(test)]
mod test {}

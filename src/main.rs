#![allow(dead_code)]
//#![feature(type_name_of_val)]
#![deny(unused_must_use)]
#![allow(unused_imports)]

use unindent::unindent;
extern crate pest;
#[macro_use]
extern crate pest_derive;

#[macro_use]
extern crate assert_json_diff;
use rand::Rng;

use serde::{Deserialize, Serialize};
use std::fmt::Write;

use futures::{Future, Stream};
use futures::{FutureExt, StreamExt};
use serde_json::{json, Value};
use std::error::Error;

use crate::launch::side_effect_run_pipeline;
use crate::parse::Env::Python;
use crate::parse::{compile, parse};
use async_std::pin::Pin;
use futures::future;
use futures::task::{Context, Poll};
use std::time::{Duration, Instant};

use anyhow;

use crate::codegen::codegen;
use crate::multiplexer::SubstreamWrite;
use crate::stream_scan::reducer_json;

mod broadcast_replay;
mod codegen;
mod dir_scan;
mod launch;
mod multiplexer;
mod parse;
mod stream_scan;
mod ws;

// addr is an enum specifying how to connect
// ideally could choose per process whether to connect as stream or as sink, where streams
// launch_pipeline(Vec<f(input_source_addr, output_sink_addr, debug_socket_addr) -> supervision_handle>)

// temporary function
//fn compile_ms(code: &str) -> Result<String, Box<dyn Error>> {
//    let parsed = parse(code);
//    let CHANGE_ME_SOON = Python;
//    let pipelines = compile(parsed.unwrap(), CHANGE_ME_SOON);
//    let res = pipelines
//        .unwrap()
//        .into_iter()
//        .map(|x| {
//            x.into_iter()
//                .map(|(env, pipeline, inp, out)| (env, codegen(env, pipeline, inp, out), inp, out))
//                .collect::<(_, _, _, _)>()
//        })
//        .collect::<String>();
//    Ok(res)
//
//    // TODO: need a linking step or we won't be able to pick connection methods efficiently... I think
//
//    // previusly in asyncmain, we did this:
//    /*
//        for pipeline in &res {
//            side_effect_run_pipeline(pipeline).await;
//        }
//    */
//}

#[derive(Clone, Serialize, Debug)]
enum CommandState {
    Editing,
    Running,
}

#[derive(Clone, Serialize, Debug)]
struct CommandBoxState {
    successful_parse: String,
    typed_text: String,
    state: CommandState,
}

#[derive(Clone, Serialize, Debug)]
struct CoreState {
    commands: Vec<CommandBoxState>,
}

// todo: fromoption<T>, provides FromOption::from_option(Option<T>) -> T which provides a default impl

#[derive(Deserialize, Debug)]
#[serde(tag = "type")]
enum CoreRecvMsg {
    Disconnect,
    // TODO: delta transmission for input? should be fairly easy
    Input { idx: usize, value: String },
    Run { idx: usize },
}

#[derive(Serialize, Debug)]
#[serde(tag = "type", content = "value")]
enum CoreSendMsg {
    State(CoreState),
    OtherMessage(i32),
}

fn core_reducer(mut state: CoreState, action: CoreRecvMsg) -> CoreState {
    println!("Action: {:?}", action);
    use CoreRecvMsg::*;
    match action {
        Input { idx, value } => {
            let mut command = &mut state.commands[idx];
            command.typed_text = value.clone();
            command.successful_parse = value;
        }
        Run { idx } => {
            let mut command = &mut state.commands[idx];
            command.state = CommandState::Running;
        }
        other => {
            println!("unhandled message type: {:?}", other);
        }
    };
    state
}

async fn run_core(mut m: multiplexer::MultiHalf<serde_json::Value>) {
    println!("launch core");
    let (mut writer, reader) = m.get_stream("_", 10).await.unwrap();

    let initial_state = CoreState {
        commands: vec![CommandBoxState {
            successful_parse: "".to_string(),
            typed_text: "".to_string(),
            state: CommandState::Editing,
        }],
    };
    writer
        .send(serde_json::to_value(CoreSendMsg::State(initial_state.clone())).unwrap())
        .await
        .unwrap();
    let mut states = reducer_json(reader, core_reducer, initial_state);

    loop {
        match states.next().await {
            Some(state) => {
                let derp: CoreSendMsg = CoreSendMsg::State(state);
                writer
                    .send(serde_json::to_value(derp).unwrap())
                    .await
                    .unwrap();
            }
            None => break,
        }
    }
    //.forward(writer);
}

#[tokio::main]
async fn main() {
    let (single, multi) = multiplexer::multiplexer(1, 1);

    tokio::spawn(async move {
        run_core(multi).await;
    });
    ws::run_websocket(single).await.unwrap();

    // workaround for deadlock in test_core_init :sweat_smile:
    //tokio::time::delay_for(Duration::from_secs(1)).await;

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
mod test {
    use super::*;
    use crate::codegen::serialize_for_python_source;
    use crate::parse::Milkaddr::StandardIn;
    use crate::parse::{Env, Milkaddr, Pipeline};

    //#[tokio::test]
    //async fn test_start_command() {
    //    let (single, multi) = multiplexer::multiplexer(1, 1);
    //    // workaround for deadlock - FIXME!
    //    let joinhandle = tokio::spawn(async move {
    //        tokio::time::timeout(Duration::from_millis(20), run_core(multi))
    //            .await
    //            .expect_err("core did not stay running")
    //    });
    //    let mut connector = broadcast_replay::launch_broadcasters(single);
    //    let (mut input, mut output) = connector.connect().await;
    //    output.recv().await.unwrap();
    //    input
    //        .send(json!({
    //            "stream_id": "_",
    //            "message": {
    //                "type": "Input",
    //                "value": "@py { print('test') } ",
    //                "idx": 0
    //            }
    //        }))
    //        .await
    //        .unwrap();

    //    output
    //        .recv()
    //        .await
    //        .expect("should be a message confirming typing waiting");

    //    input
    //        .send(json!({
    //            "stream_id": "_",
    //            "message": {
    //                "type": "Run",
    //                "idx": 0
    //            }
    //        }))
    //        .await
    //        .unwrap();

    //    let res = output.recv().await.unwrap();
    //    let stream_id: String = res
    //        .as_object()
    //        .expect("1")
    //        .get("message")
    //        .expect("2")
    //        .as_object()
    //        .expect("3")
    //        .get("value")
    //        .expect("4")
    //        .as_object()
    //        .expect("5")
    //        .get("commands")
    //        .expect("6")
    //        .as_array()
    //        .expect("7")
    //        .get(0)
    //        .expect("8")
    //        .as_object()
    //        .expect("9")
    //        .get("stream_id")
    //        .expect("a")
    //        .as_str()
    //        .expect("b")
    //        .to_string();

    //    assert_json_include!(
    //        actual: res,
    //        expected: json!({
    //            "message": {
    //                "type": "State",
    //                "value": {
    //                    "commands": [{
    //                        "successful_parse": "@py { print('test') } ",
    //                        "typed_text": "@py { print('test') } ",
    //                    }]
    //                }
    //            },
    //            "stream_id": "_"
    //        })
    //    );

    //    assert_json_eq!(
    //        output.recv().await.unwrap(),
    //        json!({
    //            "message": "test",
    //            "stream_id": stream_id
    //        })
    //    );

    //    joinhandle.await.unwrap();
    //}
    #[test]
    fn test_compile_python_2() {
        let formatted = unindent(
            "
                test
                derp|hello|there
                |whee
                yay|hello
                @py { return i_am_a_dummy_language_block(val)} | command
            ",
        );
        let parsed = parse(formatted.as_str());
        let pipelines = compile(parsed.unwrap(), Python).unwrap();
        println!("pipelines: {:?}", pipelines);
        assert_eq!(pipelines.len(), 4);
        let mapped = pipelines
            .into_iter()
            .map(|pipeline| {
                pipeline
                    .into_iter()
                    .map(|(env, pipeline, inp, out)| {
                        (env, codegen(env, pipeline, inp, out), inp, out)
                    })
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<Vec<(Env, String, Option<Milkaddr>, Option<Milkaddr>)>>>();
        assert_eq!(
            mapped,
            vec![
                vec![(
                    Python,
                    unindent(
                        "
                        import milkshell

                        def default_pipeline(val):
                            val = test(val)
                            return val
                        if __name__ == '__main__': milkshell.write_stdout(default_pipeline(milkshell.stdin()))\n"
                    ),
                    Some(StandardIn),
                    None
                )],
                vec![(
                    Python,
                    unindent(
                        "
                        import milkshell

                        def default_pipeline(val):
                            val = derp(val)
                            val = hello(val)
                            val = there(val)
                            val = whee(val)
                            return val
                        if __name__ == '__main__': milkshell.write_stdout(default_pipeline(milkshell.stdin()))"
                    ),
                    Some(StandardIn),
                    None
                )],
                vec![(
                    Python,
                    unindent(
                        "
                        import milkshell

                        def default_pipeline(val):
                            val = yay(val)
                            val = hello(val)
                            return val
                        if __name__ == '__main__': milkshell.write_stdout(default_pipeline(milkshell.stdin()))\n"
                    ),
                    Some(StandardIn),
                    None
                )],
                vec![(
                    Python,
                    unindent(
                        "
                        import milkshell

                        def v0(val):
                            return i_am_a_dummy_language_block(val)

                        def default_pipeline(val):
                            val = v0(val)
                            val = command(val)
                            return val
                        if __name__ == '__main__': milkshell.write_stdout(default_pipeline(milkshell.stdin()))\n"
                    ),
                    Some(StandardIn),
                    None
                )]
            ]
        );
    }

    #[tokio::test]
    async fn test_run_python() {
        use std::fs;

        let mut rng = rand::thread_rng();
        let n2: u16 = rng.gen();
        let formatted = format!("@py {{ return ({} for x in range(10)) }} | @py {{ open('/tmp/milkshell_test_out', 'w').write(''.join(str(v) + '\\n' for v in val)) }}", n2);
        let parsed = parse(formatted.as_str()).unwrap();
        let compiled = compile(parsed, Env::Python).unwrap();

        let _deleted = fs::remove_file("/tmp/milkshell_test_out").is_ok();

        for pipeline in compiled {
            side_effect_run_pipeline(pipeline).await.unwrap();
        }
        let result = fs::read_to_string("/tmp/milkshell_test_out").unwrap();
        assert_eq!(result, format!("{}\n", n2).repeat(10));
        let _deleted = fs::remove_file("/tmp/milkshell_test_out").is_ok();

        // current code:
        //let res = compile(
        //    parse(
        //        r##"
        //            map @py {
        //                v + 1
        //            } | @py { import derp; print("hello") } around the world
        //        "##,
        //    )
        //    .unwrap(),
        //    Python,
        //)
        //.unwrap();
        //for pipeline in &res {
        //    side_effect_run_pipeline(pipeline);
        //}
    }

    #[test]
    fn test_compile_python() {
        let parsed = parse("test\nderp|hello|there\n|whee\nyay|hello");
        let pipelines = compile(parsed.unwrap(), Python).unwrap();
        println!("pipelines: {:?}", pipelines);
        assert_eq!(pipelines.len(), 3);
        let res = pipelines
            .into_iter()
            .flat_map(|x| {
                x.into_iter()
                    .map(|(_env, pipeline, _inp, _out)| serialize_for_python_source(pipeline))
            })
            .collect::<Vec<(String, String)>>();
        assert_eq!(
            res.iter()
                .map(|(x, y)| (x.as_str(), y.as_str()))
                .collect::<Vec<_>>(),
            vec![
                ("", "    val = test(val)"),
                (
                    "",
                    "    val = derp(val)\n    val = hello(val)\n    val = there(val)\n    val = whee(val)"
                ),
                (
                    "",
                    "    val = yay(val)\n    val = hello(val)"
                )
            ]
        );
    }

    //#[test]
    //fn test_compile_python() {
    //    let parsed = parse("test");
    //    let CHANGE_ME_SOON = Python;
    //    let pipelines = compile(parsed.unwrap(), CHANGE_ME_SOON).unwrap();
    //    assert_eq!(pipelines.len(), 1);
    //    let res = pipelines
    //        .into_iter()
    //        .flat_map(|x| {
    //            x.into_iter()
    //                .map(|(env, pipeline, inp, out)| serialize_for_python_source(pipeline))
    //        })
    //        .collect::<Vec<(String, String)>>();
    //    assert_eq!(
    //        res,
    //        vec![("".to_string(), "    val = test(val)".to_string())]
    //    );
    //}

    #[tokio::test]
    async fn test_parse_command() {
        let (single, multi) = multiplexer::multiplexer(1, 1);
        // workaround for deadlock - FIXME!
        let joinhandle = tokio::spawn(async move {
            tokio::time::timeout(Duration::from_millis(20), run_core(multi))
                .await
                .expect_err("core did not stay running")
        });
        let mut connector = broadcast_replay::launch_broadcasters(single);
        let (mut input, mut output) = connector.connect().await;
        let first_message = output.recv().await.unwrap();

        assert_json_include!(
            actual: first_message,
            expected: json!({
                "stream_id": "_",
                "message": {
                    "type": "State",
                    "value": {

                    "commands": [{
                        "typed_text": "",
                    }]
                    }
                }
            })
        );

        input
            .send(json!({
                "stream_id": "_",
                "message": {
                    "type": "Input",
                    "value": "test",
                    "idx": 0
                }
            }))
            .await
            .unwrap();

        assert_json_include!(
            actual: output.recv().await.expect("a message should be waiting"),
            expected: json!({
                "message": {
                    "type": "State",
                    "value": {
                        "commands": [{
                            "typed_text": "test",
                        }]
                    }
                },
                "stream_id": "_"
            })
        );

        joinhandle.await.unwrap();
    }
}

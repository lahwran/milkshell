#![allow(dead_code)]
#![deny(unused_must_use)]

extern crate pest;
#[macro_use]
extern crate pest_derive;

#[macro_use]
extern crate lazy_static;
extern crate regex;
extern crate serde_json;
use serde_json::json;
use std::fmt::Write;

use tokio;
use tokio::process::{Child, Command};

extern crate unindent;

use unindent::unindent;

use regex::Regex;

use pest::iterators::Pair;
use pest::Parser;

#[derive(Parser)]
#[grammar = "milkshell.pest"]
pub struct MilkshellParser;

use crate::ArgumentValue::{CommandReference, LanguageBlock, PlainString, VariableReference};
use crate::Env::{Javascript, Python};
use crate::SharedInvocationPipe::StandardIn;
use futures::Future;
use std::error::Error;
use std::fs::File;
use std::path::Path;
use std::{fs, iter};
use std::{io, thread, time};

use futures::future::join_all;
use std::time::{Duration, Instant};
use tokio::runtime::Runtime;

mod multiplexer;
mod ws;

// one possible implementation of walking a directory only visiting files
fn visit_dirs(dir: &Path, cb: &dyn Fn(&Path)) -> io::Result<()> {
    if dir.is_dir() {
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() {
                visit_dirs(&path, cb)?;
            } else {
                cb(&path)
            }
        }
    }
    Ok(())
}

fn visit_filtered(dir: &Path, ext: &str, cb: &dyn Fn(&Path, &str)) -> io::Result<()> {
    visit_dirs(dir, &|path: &Path| {
        let v = path.file_name().unwrap().to_str().unwrap();
        if v.ends_with(ext) {
            match fs::read_to_string(path) {
                Ok(contents) => cb(path, &contents),
                Err(e) => println!("Error reading file {:?}: {:?}", path, e),
            }
        }
    })
}

fn parse(string: &str) -> Result<Pair<Rule>, pest::error::Error<Rule>> {
    let res = MilkshellParser::parse(Rule::milkshell, string)?
        .next()
        .unwrap();
    Ok(res)
}

// is the name "concrete syntax tree" dumb? idk but it seems right to me so I'm using it. that's
// what "cst" stands for, in any event.
fn compile(
    cst: Pair<Rule>,
    default_env: Env,
) -> Result<Vec<Vec<SharedInvocation>>, Box<dyn Error>> {
    //println!("compile cst {:?}", cst);
    Ok(cst
        .into_inner()
        .filter_map(|pipeline| {
            //println!(" => {:?}", pipeline);
            match pipeline.as_rule() {
                Rule::ews_pipeline => Some(prepare_pipeline(pipeline, default_env)),
                _ => None,
            }
        })
        .collect::<Vec<_>>())
}

#[derive(Debug)]
enum ArgumentValue {
    LanguageBlock(String, Env),
    CommandReference(String),
    VariableReference(String),
    PlainString(String),
}

// todo: replace with typed things
#[derive(Debug)]
struct Op {
    operator: ArgumentValue,
    arguments: Vec<ArgumentValue>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Env {
    Python,
    Javascript,
}
#[derive(Debug, Clone, Copy)]
enum SharedInvocationPipe {
    StandardIn,
    StandardOut,
    InternalConnection,
    IPC,
}
#[derive(Debug)]
struct SharedInvocation {
    environment: Env,
    ops: Vec<Op>,
    pipe_in: Option<SharedInvocationPipe>,
    pipe_out: Option<SharedInvocationPipe>,
}
#[derive(Debug)]
struct ConcreteSharedInvocation {
    environment: Env,
    code: String,
    pipe_in: Option<SharedInvocationPipe>,
    pipe_out: Option<SharedInvocationPipe>,
}

fn prepare_double_string(double_string: Pair<Rule>, _env: Env) -> (ArgumentValue, Option<Env>) {
    // TODO: parse, extract, process dynamic values in double strings (...maybe)
    // TODO: process escapes
    // TODO: remove quotes
    (
        ArgumentValue::PlainString(
            double_string
                .into_inner()
                .next()
                .unwrap()
                .as_str()
                .to_string(),
        ),
        None,
    )
}

fn prepare_operator(token: Pair<Rule>, env: Env) -> (ArgumentValue, Option<Env>) {
    //println!("         => operator: {}", operator);
    match token.as_rule() {
        Rule::language_block => {
            let lb = token.into_inner().next().unwrap();
            let (block_string, new_env) = match lb.as_rule() {
                Rule::python_block => (lb.into_inner().next().unwrap().as_str(), Python),
                Rule::js_block => (lb.into_inner().next().unwrap().as_str(), Javascript),
                _ => panic!("unhandled language block type"),
            };
            (
                ArgumentValue::LanguageBlock(block_string.to_string(), new_env),
                Some(new_env),
            )
        }
        Rule::double_string => prepare_double_string(token.into_inner().next().unwrap(), env),
        Rule::single_string => (
            ArgumentValue::PlainString(token.into_inner().next().unwrap().as_str().to_string()),
            None,
        ), // todo: strip quotes
        Rule::plain_word => (ArgumentValue::PlainString(token.as_str().to_string()), None),
        _ => panic!("Unhandled argument type, add to prepare_operator"),
    }
}

fn prepare_argument(token: Pair<Rule>, env: Env) -> (ArgumentValue, Option<Env>) {
    //println!("         => argument: {}", argument);
    // TODO: return language block struct or something, changing environment when needed
    //println!("         => operator: {}", operator);
    match token.as_rule() {
        Rule::language_block => {
            let lb = token.into_inner().next().unwrap();
            let (block_string, new_env) = match lb.as_rule() {
                Rule::python_block => (lb.into_inner().next().unwrap().as_str(), Python),
                Rule::js_block => (lb.into_inner().next().unwrap().as_str(), Javascript),
                _ => panic!("unhandled language block type"),
            };
            assert_eq!(env, new_env);
            (
                ArgumentValue::LanguageBlock(block_string.to_string(), new_env),
                None,
            )
        }
        Rule::double_string => prepare_double_string(token.into_inner().next().unwrap(), env),
        Rule::single_string => (
            ArgumentValue::PlainString(token.into_inner().next().unwrap().as_str().to_string()),
            None,
        ), // todo: strip quotes
        Rule::plain_word => (ArgumentValue::PlainString(token.as_str().to_string()), None),
        _ => panic!("Unhandled argument type, add to prepare_operator"),
    }
}

// plot { milkshell expression on x }
// plot @py { python expression on x }
// plot
fn prepare_command(command: Pair<Rule>, env: Env) -> (Op, Env) {
    let mut inner = command.into_inner();
    let (operator, new_env) = prepare_operator(
        inner
            .next()
            .expect("missing command")
            .into_inner()
            .next()
            .unwrap(),
        env,
    );
    let mut arguments = Vec::new();
    let mut resulting_env = match new_env {
        Some(x) => x,
        _ => env,
    };
    for arg_pair in inner {
        let (arg, new_arg_env) =
            prepare_argument(arg_pair.into_inner().next().unwrap(), resulting_env);
        resulting_env = match (new_arg_env, new_env) {
            (Some(x), Some(y)) => {
                assert_eq!(x, y);
                y
            }
            (_, Some(y)) => y,
            _ => resulting_env,
        };
        arguments.push(arg);
    }
    let op = Op {
        operator,
        arguments,
    };
    (op, resulting_env)
}

fn prepare_pipeline(pipeline: Pair<Rule>, env: Env) -> Vec<SharedInvocation> {
    // TODO: environment configuration blocks and directives
    let mut grouped: Vec<SharedInvocation> = Vec::new();
    grouped.push(SharedInvocation {
        environment: env,
        ops: Vec::new(),
        pipe_in: Some(StandardIn), // TODO: allow marking commands as "no input" somehow, to avoid opening stdin unnecessarily
        pipe_out: None,            // TODO: ditto - avoid opening stdout unnecessarily
    });
    // TODO: we might actually want to require explicitly changing default languages rather than implicit on using a non default language
    for pair in pipeline.into_inner() {
        //println!("     => pipeline item: {}", pair);
        match pair.as_rule() {
            Rule::command => {
                let (op, new_env) = prepare_command(pair, env);
                if new_env != env {
                    grouped.last_mut().unwrap().pipe_out = Some(SharedInvocationPipe::IPC);
                    grouped.push(SharedInvocation {
                        environment: new_env,
                        ops: Vec::new(),
                        pipe_in: Some(SharedInvocationPipe::IPC), // TODO: allow marking commands as "no input" somehow, to avoid opening stdin unnecessarily
                        pipe_out: None, // TODO: ditto - avoid opening stdout unnecessarily
                    })
                }
                grouped.last_mut().unwrap().ops.push(op);
            }
            Rule::pipe => {
                // process more complex pipes
            }
            _ => panic!(format!(
                "Unexpected rule in pipeline, unexpected rule was: {}",
                pair
            )),
        }
    }

    grouped
    //    let mut last_pipe = None;
    //    let mut
    //    for child in pipeline {
    //        match child.as_rule() {
    //            Rule::command => {
    //
    //            },
    //            Rule::pipe => {
    //                let pipe = prepare_pipe();
    //
    //            }
    //        }
    //    }
}

fn walk(pair: Pair<Rule>, depth: usize) {
    println!(
        "{}{:?}: {}",
        " ".repeat(depth * 2),
        pair.as_rule(),
        pair.as_str()
    );
    for _x in pair.into_inner().map(|x| walk(x, depth + 1)) {}
}

fn print_milkshell_ast(file: &str) -> Result<(), pest::error::Error<Rule>> {
    println!("start");
    let ms = parse(file)?;
    println!("done parsing");
    walk(ms, 0);
    Ok(())
}

fn get_first_langblock(pair: Pair<Rule>) -> Option<String> {
    match pair.as_rule() {
        Rule::javascript => Some(pair.as_str().to_string()),
        Rule::python => Some(pair.as_str().to_string()),
        _ => {
            for inner in pair.into_inner().map(|x| get_first_langblock(x)) {
                match inner {
                    Some(v) => return Some(v),
                    _ => (),
                }
            }
            None
        }
    }
}

async fn test_python() {
    let pypath = String::from_utf8(
        Command::new("python3")
            .arg("-c")
            .arg("import sys; print('\\n'.join(sys.path))")
            .output()
            .await
            .expect("Failed to execute command")
            .stdout,
    )
    .expect("invalid utf-8");
    for dir in pypath.split("\n") {
        visit_filtered(Path::new(dir), ".py", &|path, _contents| {
            let inner = "    \n".to_string() + _contents + "    ";
            let s = "@py {".to_string() + &inner + "}";
            let ms = parse(&s).expect(&format!("parsing failed on file {:?}", path));
            let lb = get_first_langblock(ms);
            assert_eq!(lb, Some(inner));
            println!("{:?}", path);
        })
        .unwrap();
    }
}

fn test_js() {
    visit_filtered(Path::new("../"), ".js", &|path, _contents| {
        let path_str = path.to_str().unwrap();
        let thresh = 100_000;
        if _contents.len() > thresh {
            println!(
                "skipping file file://{}, length {} greater than threshold {}",
                path_str,
                _contents.len(),
                thresh
            );
            return;
        }
        // exclude these files because they're known to be invalid! hooray.
        if path.ends_with("async-map-ordered.js") {
            // async-map-ordered.js is literally unparseable
            // /test/object/, is-accessor-descriptor, /qs/test/, xmldom/sax.js,  contain non-ascii chars

            println!("File known to not be valid! skipping: file://{}", path_str);
            return;
        }
        if !_contents.chars().all(|c| c.is_ascii()) {
            println!(
                "file contains non-ascii chars! skipping: file://{}",
                path_str
            );
            return;
        }
        let replaced = _contents.replace("\t", "    ").replace("\r", "");
        let inner = "    \n".to_string() + &replaced + "\n    ";
        let s = "@js {".to_string() + &inner + "}";
        let ms = match parse(&s) {
            Ok(v) => v,
            Err(e) => {
                println!("{}", e);
                panic!(format!("parse error on path file://{}", path_str));
            }
        };
        let lb = get_first_langblock(ms);
        assert_eq!(lb, Some(inner));
        println!("{:?}", path_str);
    })
    .unwrap();
}
fn codegen_javascript(si: &SharedInvocation) -> ConcreteSharedInvocation {
    panic!("implement for js");
}

fn gen_python_arg() {}

// TODO: verify safety
fn escape_string_python(s: &str) -> String {
    serde_json::to_string(s).expect("Escape_string_python is immune to your existence")
}

fn strip_indent(code: &str) -> String {
    let mut stripped = "".to_string();
    let mut indent = None;

    for line in code.split("\n") {
        let line_indent = line.find(|x| x != ' ');
        match (line_indent, indent) {
            (None, _) => (),
            (Some(line_indent), Some(indent)) => {
                assert!(line_indent >= indent);
            }
            (Some(line_indent), None) => indent = Some(line_indent),
        };
        if let None = line_indent {
            write!(stripped, "\n").expect("write failed");
        } else {
            let (_, after) = line.split_at(indent.unwrap());

            write!(stripped, "{}{}\n", " ".repeat(4).as_str(), after).expect("write failed");
        }
        //prelude +=
    }
    stripped
}

fn serialize_argument_for_python_source(arg: &ArgumentValue, prelude: &mut String) -> String {
    match arg {
        PlainString(val) => escape_string_python(val),
        CommandReference(val) => format!("milkshell.lookup_command({})", escape_string_python(val)),
        VariableReference(val) => {
            format!("milkshell.lookup_variable({})", escape_string_python(val))
        }
        LanguageBlock(code, _environ) => {
            let name = format!("v{}", prelude.len());
            write!(
                prelude,
                "@milk.language_block\ndef {}(seq):\n{}",
                name,
                strip_indent(code)
            )
            .expect("write failed");

            name
        }
    }
}

// TODO: move this into something serde/language target agnostic/host environment agnosting/etc
fn serialize_for_python_source(si: &SharedInvocation) -> (String, String) {
    let mut prelude = "".to_string();
    let val = format!(
        "({},)",
        si.ops
            .iter()
            .map(|op| {
                format!(
                    "({},)",
                    iter::once(&op.operator)
                        .chain(op.arguments.iter())
                        .map(|x| serialize_argument_for_python_source(x, &mut prelude))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            })
            .collect::<Vec<_>>()
            .join(", ")
    );
    return (prelude, val);
}

fn codegen_python(si: &SharedInvocation) -> ConcreteSharedInvocation {
    let (prelude, default_command) = serialize_for_python_source(si);
    for (idx, op) in si.ops.iter().enumerate() {
        for (arg_idx, arg) in op.arguments.iter().enumerate() {
            gen_python_arg();
        }
    }
    let code = format!(
        "\
         import milkshell\n\
         import trio\n\
         import sys\n\n\
         milk = milkshell.Module()\n\n\
         {}\n\n\
         milk.default_pipeline = {}\n\
         if __name__ == '__main__': milk.main(sys.argv)\
         ",
        prelude, default_command
    );
    ConcreteSharedInvocation {
        environment: si.environment,
        code,
        pipe_in: si.pipe_in,
        pipe_out: si.pipe_out,
    }
}

fn codegen(si: &SharedInvocation) -> ConcreteSharedInvocation {
    match si.environment {
        Python => codegen_python(si),
        Javascript => codegen_javascript(si),
    }
}

const PYTHON_BIN: &'static str = "/Users/lahwran/milkshell/python/.venv/bin/python";
const PYTHON_WORKDIR: &'static str = "/Users/lahwran/milkshell/python/";

fn tempfile(code: &str, idx: usize, ext: &str) -> String {
    use std::io::Write;
    // TODO: allow shutting this writing to tempfile business off and using ram only for eg python where this is possible
    // TODO: allow specifying which directory to use for tempfiles
    // TODO: manage deletion of tempfiles better - to start with, do it at all, eg after successful run?
    fs::create_dir_all("temp").expect("Creating dir failed");
    let filename = format!("temp/codegen_{}{}", idx, ext);
    let p = Path::new(&filename).canonicalize().unwrap();
    let mut f = File::create(&p).expect("Creating tempfile failed");
    f.write_all(code.as_bytes())
        .expect("writing to file failed");
    p.to_str()
        .expect("Could not convert filename to utf-8 string")
        .to_string()
}

async fn side_effect_run_pipeline(pipeline: &Vec<SharedInvocation>) {
    let launched_processes = pipeline
        .iter()
        .map(codegen)
        .enumerate()
        .map(|(idx, code)| {
            let debug_connector = "json:newlines:fd:5,6";
            let previous_connector = "json:newlines:fd:3";
            let next_connector = "json:newlines:fd:4";
            let filename = tempfile(&code.code, idx, ".py");
            let cmd = "default";
            println!(
                "running: <python> {:?} {:?} {:?} {:?} {:?}",
                filename, cmd, previous_connector, next_connector, debug_connector
            );
            // TODO: open fds 3:w,4:r,5:w,6:r as pipes, like one normally would with fds 0:w,1:r,2:r

            Command::new(PYTHON_BIN)
                .arg(filename)
                .arg(cmd)
                .arg(previous_connector)
                .arg(next_connector)
                .arg(debug_connector)
                .spawn()
        })
        .collect::<Result<Vec<_>, _>>()
        .unwrap();

    join_all(launched_processes).await;

    //println!("run:\n{:?}", collected);
    // 0. determine connection strategy, generate link instructions
    // 1. compile
    //      - locate(/connect) to compiler environment
    //      - run compiler
    // 2. run
    //      - connect to execution environment
    //      - run binary
}
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

async fn run_core(m: multiplexer::MultiHalf<serde_json::Value>) {
    println!("launch core");
    let (writer, reader) = m.get_stream("default").unwrap();
    loop {
        println!("send... {:?}", Instant::now());
        writer
            .send(json!({"hello": "there", "it is": format!("{:?}", Instant::now())}))
            .await;
        println!("delay... {:?}", Instant::now());
        tokio::time::delay_for(Duration::from_secs(3)).await
    }
}

#[tokio::main]
async fn main() {
    //let (single, multi) = multiplexer::multiplexer(1, 1);

    //tokio::spawn(async move {
    //    run_core(multi).await;
    //});
    //ws::run_websocket(single).await.unwrap()

    use std::time::Duration;

    use async_std::sync::channel;
    use async_std::task;

    let (s, r) = channel(1);
    let r2 = r.clone();

    // This call returns immediately because there is enough space in the channel.
    s.send(1).await;

    task::spawn(async move {
        // This call will have to wait because the channel is full.
        // It will be able to complete only after the first message is received.
        s.send(2).await;
    });

    task::sleep(Duration::from_secs(1)).await;
    assert_eq!(r.recv().await, Some(1));
    assert_eq!(r.recv().await, Some(2));
    assert_eq!(r2.recv().await, Some(1));
    assert_eq!(r2.recv().await, Some(2));

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

//fn main() {
//println!("main thread starts.");
//thread::spawn(move || {
//println!("io thread starts.");
//let mut rt = Runtime::new().unwrap();
//rt.block_on(asyncmain_wrapper());
//println!("io thread complete.");
//});
//println!("main thread launched io thread.");
//for idx in 0..100 {
//    println!("iter {:?}/100 of waiter loop. waiting 1 seconds...", idx);
//    let dur = time::Duration::from_secs(1);
//    thread::sleep(dur);
//}

//println!("Main thread complete.");
//}

fn oldmain() {
    //let s = "@py { derp derp derp }";
    //parse_milkshell(s).unwrap();
    //test_python();
    //test_js();
    // @py { function_name }
    // @py { operation on input per message }
    // @py { multiline operation on input, with wait on other operations }
    // @py { for loop over input messages }

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
    //println!("compile result: {:?}", &res)

    //net::net_test_main().unwrap();
}

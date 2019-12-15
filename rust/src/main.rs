#![allow(dead_code)]
#![deny(unused_must_use)]


extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::iterators::Pair;
use pest::Parser;

#[derive(Parser)]
#[grammar = "milkshell.pest"]
pub struct MilkshellParser;

use crate::Env::{Javascript, Python};
use crate::SharedInvocationPipe::StandardIn;
use std::error::Error;
use std::fs;
use std::io;
use std::path::Path;
use std::process::Command;

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
#[derive(Debug)]
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

fn prepare_argument(argument: Pair<Rule>, _env: Env) -> (ArgumentValue, Option<Env>) {
    //println!("         => argument: {}", argument);
    // TODO: return language block struct or something, changing environment when needed
    (
        ArgumentValue::PlainString(argument.as_str().to_string()),
        None,
    )
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

fn test_python() {
    let pypath = String::from_utf8(
        Command::new("python3")
            .arg("-c")
            .arg("import sys; print('\\n'.join(sys.path))")
            .output()
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

fn side_effect_run_pipeline(pipeline: &Vec<SharedInvocation>) {

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

fn main() {
    //let s = "@py { derp derp derp }";
    //parse_milkshell(s).unwrap();
    //test_python();
    //test_js();
    // @py { function_name }
    // @py { operation on input per message }
    // @py { multiline operation on input, with wait on other operations }
    // @py { for loop over input messages }

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
        side_effect_run_pipeline(pipeline);
    }
    println!("compile result: {:?}", &res)
}

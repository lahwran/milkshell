use pest::iterators::Pair;
use pest::Parser;
use std::error::Error;

#[derive(Parser)]
#[grammar = "milkshell.pest"]
pub struct MilkshellParser;

pub(crate) fn parse(string: &str) -> Result<Pair<Rule>, pest::error::Error<Rule>> {
    let res = MilkshellParser::parse(Rule::milkshell, string)?
        .next()
        .unwrap();
    Ok(res)
}

// is the name "concrete syntax tree" dumb? idk but it seems right to me so I'm using it. that's
// what "cst" stands for, in any event.
pub(crate) fn compile(
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
pub(crate) enum ArgumentValue {
    LanguageBlock(String, Env),
    CommandReference(String),
    VariableReference(String),
    PlainString(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Env {
    Python,
    Javascript,
}
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub(crate) enum Milkaddr {
    StandardIn,
    StandardOut,
    InternalConnection,
    IPC,
}

pub(crate) type Pipeline = Vec<(ArgumentValue, Vec<ArgumentValue>)>;

pub(crate) type SharedInvocation = (Env, Pipeline, Option<Milkaddr>, Option<Milkaddr>);

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
                Rule::python_block => (lb.into_inner().next().unwrap().as_str(), Env::Python),
                Rule::js_block => (lb.into_inner().next().unwrap().as_str(), Env::Javascript),
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
                Rule::python_block => (lb.into_inner().next().unwrap().as_str(), Env::Python),
                Rule::js_block => (lb.into_inner().next().unwrap().as_str(), Env::Javascript),
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
fn prepare_command(command: Pair<Rule>, env: Env) -> (ArgumentValue, Vec<ArgumentValue>, Env) {
    let mut inner = command.into_inner();
    let (operator, new_env) = prepare_operator(
        inner
            .next()
            .expect("missing command")
            .into_inner()
            .next()
            .unwrap(),
        env.clone(),
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
            _ => resulting_env.clone(),
        };
        arguments.push(arg);
    }
    (operator, arguments, resulting_env)
}

fn prepare_pipeline(pipeline: Pair<Rule>, env: Env) -> Vec<SharedInvocation> {
    // TODO: environment configuration blocks and directives
    let mut grouped: Vec<SharedInvocation> = Vec::new();
    grouped.push(((&env).clone(), Vec::new(), Some(Milkaddr::StandardIn), None));
    // TODO: we might actually want to require explicitly changing default languages rather than implicit on using a non default language
    for pair in pipeline.into_inner() {
        //println!("     => pipeline item: {}", pair);
        match pair.as_rule() {
            Rule::command => {
                let (op, args, new_env) = prepare_command(pair, env);
                if new_env != env {
                    grouped.last_mut().unwrap().3 = Some(Milkaddr::IPC);
                    grouped.push((new_env.clone(), Vec::new(), Some(Milkaddr::IPC), None));
                }
                grouped.last_mut().unwrap().1.push((op, args));
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

#[cfg(test)]
mod test {
    use super::*;

    use crate::dir_scan::visit_filtered;
    use pest::iterators::Pair;
    use std::path::Path;
    use tokio::process::Command;

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

    #[tokio::test]
    #[ignore]
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

    #[test]
    #[ignore]
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
}

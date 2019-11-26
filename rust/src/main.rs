
#![allow(dead_code)]
#![deny(unused_must_use)]

extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::Parser;
use pest::iterators::Pair;

#[derive(Parser)]
#[grammar = "milkshell.pest"]
pub struct MilkshellParser;

use std::io;
use std::fs;
use std::path::Path;
use std::process::Command;
use pest::error::ErrorVariant::ParsingError;

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
    visit_dirs(dir, & |path: &Path| {
        let v = path.file_name().unwrap().to_str().unwrap();
        if v.ends_with(ext) {
            match fs::read_to_string(path) {
                Ok(contents) => cb(path, &contents),
                Err(e) =>  println!("Error reading file {:?}: {:?}", path, e)
            }

        }
    })
}


fn parse(string: &str) -> Result<Pair<Rule>, pest::error::Error<Rule>> {
    let res = MilkshellParser::parse(Rule::milkshell, string)?.next().unwrap();
    Ok(res)
}

fn walk(pair: Pair<Rule>, depth: usize) {
    println!("{}{:?}: {}", " ".repeat(depth*2), pair.as_rule(), pair.as_str());
    for _x in pair.into_inner().map(|x| { walk(x, depth+1) } ) {}
}

fn parse_milkshell(file: &str) -> Result<(), pest::error::Error<Rule>> {
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
            for inner in pair.into_inner().map(|x| { get_first_langblock(x) } ) {
                match inner {
                    Some(v) => return Some(v),
                    _ => ()
                }
            }
            None
        }
    }
}

fn test_python() {
    let pypath = String::from_utf8(Command::new("python3")
        .arg("-c")
        .arg("import sys; print('\\n'.join(sys.path))")
        .output()
        .expect("Failed to execute command")
        .stdout).expect("invalid utf-8");
    for dir in pypath.split("\n") {
        visit_filtered(Path::new(dir), ".py", &|path, _contents| {
            let inner = "    \n".to_string() + _contents + "    ";
            let s = "@py {".to_string() + &inner + "}";
            let ms = parse(&s).expect(&format!("parsing failed on file {:?}", path));
            let lb = get_first_langblock(ms);
            assert_eq!(lb, Some(inner));
            println!("{:?}", path);
        }).unwrap();
    }
}

fn test_js() {
    visit_filtered(Path::new("../"), ".js", &|path, _contents| {
        let path_str = path.to_string_lossy();
        let thresh = 100_000;
        if _contents.len() > thresh {
            println!("skipping file file://{}, length {} greater than threshold {}", path.to_str().unwrap(), _contents.len(), thresh);
            return;
        }
        // exclude these files because they're known to be invalid! hooray.
        if path.ends_with("async-map-ordered.js")  {
            // async-map-ordered.js is literally unparseable
            // /test/object/, is-accessor-descriptor, /qs/test/, xmldom/sax.js,  contain non-ascii chars

            println!("File known to not be valid! skipping: file://{}", path.to_str().unwrap());
            return;
        }
        if !_contents.chars().all(|c| c.is_ascii()) {
            println!("file contains non-ascii chars! skipping: file://{}", path.to_str().unwrap());
            return;
        }
        let inner = "    \n".to_string() + &_contents.replace("\t", "    ").replace("\r", "") + "\n    ";
        let s = "@js {".to_string() + &inner + "}";
        let ms = match parse(&s) {
            Ok(v) => v,
            Err(e) => {
                println!("{}", e);
                panic!(format!("parse error on path file://{}", path.to_str().unwrap()));
            }
        };
        let lb = get_first_langblock(ms);
        assert_eq!(lb, Some(inner));
        println!("{:?}", path);
    }).unwrap();
}

fn main() {
    let s = "@py { derp derp derp }";
    parse_milkshell(s).unwrap();
    test_python();
    test_js();
}

use crate::codegen::codegen;
use crate::parse::SharedInvocation;
use futures::future::join_all;
use std::fs;
use std::fs::File;
use std::path::Path;
use tokio;
use tokio::process::Command;

const PYTHON_BIN: &'static str = "/Users/lahwran/milkshell/python/.venv/bin/python";
const PYTHON_WORKDIR: &'static str = "/Users/lahwran/milkshell/python/";

fn tempfile(code: &str, idx: usize, ext: &str) -> String {
    use std::io::Write;
    // TODO: allow shutting this writing to tempfile business off and using ram only for eg python where this is possible
    // TODO: allow specifying which directory to use for tempfiles
    // TODO: manage deletion of tempfiles better - to start with, do it at all, eg after successful run?
    fs::create_dir_all("temp").expect("Creating dir failed");
    let filename = format!("temp/codegen_{}{}", idx, ext);
    let p = Path::new(&filename);
    let mut f = File::create(&p).expect("Creating tempfile failed");
    f.write_all(code.as_bytes())
        .expect("writing to file failed");
    p.canonicalize()
        .unwrap()
        .to_str()
        .expect("Could not convert filename to utf-8 string")
        .to_string()
}

pub(crate) async fn side_effect_run_pipeline(
    pipeline: Vec<SharedInvocation>,
) -> std::result::Result<std::vec::Vec<std::process::ExitStatus>, std::io::Error> {
    let launched_processes = pipeline
        .into_iter()
        .map(|(env, pipeline, inp, out)| (env, codegen(env, pipeline, inp, out), inp, out))
        .enumerate()
        .map(|(idx, (_, code, _, _))| {
            let debug_connector = "json:newlines:fd:5,6";
            let previous_connector = "json:newlines:fd:3";
            let next_connector = "json:newlines:fd:4";
            let filename = tempfile(&code, idx, ".py");
            let cmd = "default";

            println!(
                "running: {:?} {:?} {:?} {:?} {:?} {:?}",
                PYTHON_BIN, filename, cmd, previous_connector, next_connector, debug_connector
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

    let a: Result<Vec<_>, _> = join_all(launched_processes).await.into_iter().collect();
    a

    //println!("run:\n{:?}", collected);
    // 0. determine connection strategy, generate link instructions
    // 1. compile
    //      - locate(/connect) to compiler environment
    //      - run compiler
    // 2. run
    //      - connect to execution environment
    //      - run binary
}

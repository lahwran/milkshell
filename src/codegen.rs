use crate::parse::ArgumentValue::{
    CommandReference, LanguageBlock, PlainString, VariableReference,
};
use crate::parse::{ArgumentValue, Env, Milkaddr, Pipeline, SharedInvocation};
use std::fmt::Write;
use std::{fs, iter};
use unindent::unindent;

fn codegen_javascript(
    env: Env,
    pipeline: Pipeline,
    inp: Option<Milkaddr>,
    out: Option<Milkaddr>,
) -> String {
    panic!("implement for js");
}

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

fn serialize_op_for_python_source(arg: &ArgumentValue, prelude: &mut String) -> String {
    match arg {
        PlainString(val) | CommandReference(val) => val.clone(),
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

fn serialize_argument_for_python_source(arg: &ArgumentValue, prelude: &mut String) -> String {
    match arg {
        PlainString(val) => escape_string_python(val),
        CommandReference(val) => val.clone(),
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
pub(crate) fn serialize_for_python_source(
    ops: Vec<(ArgumentValue, Vec<ArgumentValue>)>,
) -> (String, String) {
    let mut prelude = "".to_string();

    let val = ops
        .iter()
        .map(|(op, args)| {
            format!(
                "    val = {}({})",
                serialize_op_for_python_source(op, &mut prelude),
                iter::once(&CommandReference("val".to_string()))
                    .chain(args.iter())
                    .map(|x| serialize_argument_for_python_source(x, &mut prelude))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        })
        .collect::<Vec<_>>()
        .join("\n");

    (prelude, val)
}

fn codegen_python(
    env: Env,
    pipeline: Pipeline,
    inp: Option<Milkaddr>,
    out: Option<Milkaddr>,
) -> String {
    let (prelude, default_command) = serialize_for_python_source(pipeline);
    unindent(&format!(
        "
         import milkshell
         {prelude}
         def default_pipeline(val):
         {default_command}
             return val
         if __name__ == '__main__': milkshell.run(default_pipeline)
         ",
        /*
             inp, out_writer = milkshell.connect_pair(sys.argv[1], sys.argv[2])\n\
             res = default_pipeline(inp)\n\
             milkshell.sendall(res, out_writer)\n\
        */
        prelude = prelude,
        default_command = default_command
    ))
}

pub(crate) fn codegen(
    env: Env,
    pipeline: Pipeline,
    inp: Option<Milkaddr>,
    out: Option<Milkaddr>,
) -> String {
    match &env {
        Env::Python => codegen_python(env, pipeline, inp, out),
        Env::Javascript => codegen_javascript(env, pipeline, inp, out),
    }
}

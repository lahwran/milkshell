use crate::parse::ArgumentValue::{
    CommandReference, LanguageBlock, PlainString, VariableReference,
};
use crate::parse::{ArgumentValue, Env, SharedInvocation, SharedInvocationPipe};
use std::fmt::Write;
use std::{fs, iter};

#[derive(Debug)]
pub(crate) struct ConcreteSharedInvocation {
    environment: Env,
    pub(crate) code: String,
    pipe_in: Option<SharedInvocationPipe>,
    pipe_out: Option<SharedInvocationPipe>,
}

fn codegen_javascript(_si: &SharedInvocation) -> ConcreteSharedInvocation {
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
    for (_idx, op) in si.ops.iter().enumerate() {
        for (_arg_idx, _arg) in op.arguments.iter().enumerate() {
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

pub(crate) fn codegen(si: &SharedInvocation) -> ConcreteSharedInvocation {
    match si.environment {
        Python => codegen_python(si),
        Javascript => codegen_javascript(si),
    }
}

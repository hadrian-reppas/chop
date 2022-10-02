mod ast;
mod builtins;
mod codegen;
mod error;
mod expr;
mod lex;
mod parse;
mod typecheck;

use std::env::args;
use std::fs::File;
use std::io::Write;

fn main() {
    let args: Vec<_> = args().collect();
    if args.len() == 2 {
        transpile(&args[1]);
    } else {
        println!("USAGE: chop <filename>");
    }
}

fn transpile(file_name: &str) {
    match parse::parse_file(file_name) {
        Ok(unit) => match typecheck::typecheck(&unit) {
            Ok(info) => {
                let mut file = File::create("out.c").unwrap();
                let code = codegen::generate(&info);
                write!(file, "{}", code).unwrap();
                /*
                std::process::Command::new("clang")
                    .args(["out.c", "-o", "out"])
                    .output()
                    .unwrap();
                */
            }
            Err(err) => err.println(),
        },
        Err(err) => err.println(),
    };
}

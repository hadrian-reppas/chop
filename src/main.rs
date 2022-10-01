mod ast;
mod builtins;
mod codegen;
mod error;
mod expr;
mod lex;
mod parse;
mod typecheck;

use std::fs::File;
use std::io::Write;
use std::process::Command;

fn main() {
    compile("examples/hello_world.hop");
}

fn compile(file_name: &str) {
    match parse::parse_file(file_name) {
        Ok(unit) => match typecheck::typecheck(&unit) {
            Ok(info) => {
                let mut file = File::create("out.c").unwrap();
                let code = codegen::generate(&unit, &info);
                write!(file, "{}", code).unwrap();
                Command::new("clang")
                    .args(["out.c", "-o", "out"])
                    .output()
                    .unwrap();
            }
            Err(err) => err.println(),
        },
        Err(err) => err.println(),
    };
}

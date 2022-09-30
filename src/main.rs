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

fn main() {
    match parse::parse_file("examples/codegen.hop") {
        Ok(unit) => {
            println!("{unit:?}");
            match typecheck::typecheck(&unit) {
                Ok(info) => {
                    println!("typechecks!");
                    let mut file = File::create("out.c").unwrap();
                    let code = codegen::generate(&unit, &info);
                    write!(file, "{}", code).unwrap();
                }
                Err(err) => err.println(),
            }
        }
        Err(err) => err.println(),
    };
}

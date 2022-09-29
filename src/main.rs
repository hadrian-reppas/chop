mod ast;
mod builtins;
mod codegen;
mod error;
mod expr;
mod lex;
mod parse;
mod typecheck;

use std::fs::File;

fn main() {
    match parse::parse_file("examples/codegen.hop") {
        Ok(unit) => {
            println!("{unit:?}");
            match typecheck::typecheck(&unit) {
                Ok(info) => {
                    println!("typechecks!");
                    let mut file = File::create("out.ll").unwrap();
                    codegen::generate(&mut file, &unit, &info).unwrap();
                }
                Err(err) => err.println(),
            }
        }
        Err(err) => err.println(),
    };
}

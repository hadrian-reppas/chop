mod ast;
mod builtins;
mod codegen;
mod error;
mod expr;
mod imports;
mod lex;
mod parse;
mod typecheck;

use std::env::args;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;
use std::process::exit;

fn main() {
    let args: Vec<_> = args().collect();
    let code = if args.len() == 2 {
        if let Err(err) = transpile(&args[1]) {
            err.println();
            1
        } else {
            0
        }
    } else {
        println!("USAGE: chop <filename>");
        1
    };
    exit(code);
}

fn transpile(file_name: &str) -> Result<(), error::Error> {
    let main_unit = parse::parse_file(file_name)?;
    let unit = imports::resolve_imports(main_unit, PathBuf::from(file_name))?;
    println!("{:#?}", unit);
    let info = typecheck::typecheck(&unit)?;
    let code = codegen::generate(&info);
    let mut file = File::create("out.c").unwrap();
    write!(file, "{}", code).unwrap();
    Ok(())
}

mod ast;
mod builtins;
mod codegen;
mod error;
mod expr;
mod imports;
mod lex;
mod parse;
mod program;
mod typecheck;
mod types;

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
    let (units, main_unit_prefix) = imports::collect(main_unit, PathBuf::from(file_name))?;
    println!("{:#?}", units.keys().collect::<Vec<_>>());
    let info = typecheck::check(&units, &main_unit_prefix)?;
    let code = codegen::generate(info);
    let mut file = File::create("out.c").unwrap();
    let len = file.write(code.as_bytes()).unwrap();
    assert!(len == code.len());
    Ok(())
}

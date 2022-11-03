mod ast;
mod builtins;
mod c_codegen;
mod error;
mod expr;
mod imports;
mod lex;
mod parse;
mod program;
mod rust_codegen;
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
    let unit = imports::resolve_imports(main_unit, PathBuf::from(file_name))?;
    let info = typecheck::check(&unit)?;
    info.display();
    let code = rust_codegen::generate(&info);
    let mut file = File::create("out.rs").unwrap();
    write!(file, "{}", code).unwrap();
    Ok(())
}

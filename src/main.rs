mod ast;
mod builtins;
mod error;
mod expr;
mod lex;
mod parse;
mod typecheck;

fn main() {
    match parse::parse_file("examples/deref.hop") {
        Ok(unit) => {
            println!("{unit:?}");
            match typecheck::typecheck(&unit) {
                Ok(_) => println!("typechecks!"),
                Err(err) => err.println(),
            }
        }
        Err(err) => err.println(),
    };
}

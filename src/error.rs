use crate::lex::Span;

use lazy_static::lazy_static;
use termion::{color, style};

lazy_static! {
    pub static ref RED: String = format!("{}{}", color::Fg(color::Red), style::Bold);
    pub static ref GREEN: String = format!("{}{}", color::Fg(color::Green), style::Bold);
    pub static ref BLUE: String = format!("{}{}", color::Fg(color::Blue), style::Bold);
    pub static ref RESET: String = format!("{}{}", color::Fg(color::Reset), style::Reset);
}

#[derive(Debug)]
pub enum Error {
    Io(String),
    Lex(Span, String),
    Parse(Span, String),
    Type(Span, String, Vec<Note>),
    Main(String, Vec<Note>),
    Import(Span, String),
}

#[derive(Debug)]
pub struct Note {
    pub span: Option<Span>,
    pub msg: String,
}

impl Note {
    pub fn new(span: Option<Span>, msg: String) -> Note {
        Note { span, msg }
    }
}

impl Error {
    pub fn print(&self) {
        match self {
            Error::Io(msg) => {
                print!("{}io error:{} {}", RED.as_str(), RESET.as_str(), msg);
            }
            Error::Lex(span, msg) => {
                println!("{}lex error:{} {msg}", RED.as_str(), RESET.as_str());
                print_span(*span);
            }
            Error::Parse(span, msg) => {
                println!("{}parse error:{} {msg}", RED.as_str(), RESET.as_str());
                print_span(*span);
            }
            Error::Type(span, msg, notes) => {
                println!("{}type error:{} {msg}", RED.as_str(), RESET.as_str());
                print_span(*span);
                for Note { span, msg } in notes {
                    if let Some(span) = span {
                        println!("\n{}note:{} {msg}", BLUE.as_str(), RESET.as_str());
                        print_span(*span);
                    } else {
                        print!("\n{}note:{} {msg}", BLUE.as_str(), RESET.as_str());
                    }
                }
            }
            Error::Main(msg, notes) => {
                print!("{}entry error:{} {msg}", RED.as_str(), RESET.as_str());
                for Note { span, msg } in notes {
                    if let Some(span) = span {
                        println!("\n{}note:{} {msg}", BLUE.as_str(), RESET.as_str());
                        print_span(*span);
                    } else {
                        print!("\n{}note:{} {msg}", BLUE.as_str(), RESET.as_str());
                    }
                }
            }
            Error::Import(span, msg) => {
                println!("{}import error:{} {msg}", RED.as_str(), RESET.as_str());
                print_span(*span);
            }
        }
    }

    pub fn println(&self) {
        self.print();
        println!();
    }
}

fn print_span(span: Span) {
    let line_num = format!("{}", span.line + 1);
    let space = " ".repeat(line_num.len());
    println!(
        "{}{}-->{} {}",
        space,
        BLUE.as_str(),
        RESET.as_str(),
        span.location()
    );

    println!("{} {}|", space, BLUE.as_str());
    println!("{} | {}{}", line_num, RESET.as_str(), span.line());
    print!(
        "{} {}| {}",
        space,
        BLUE.as_str(),
        get_carets(span.line_prefix(), span.text)
    );
}

fn get_carets(prefix: &str, text: &str) -> String {
    let white_len = prefix.chars().count();
    let caret_len = std::cmp::max(text.chars().count(), 1);
    format!(
        "{}{}{}{}",
        " ".repeat(white_len),
        RED.as_str(),
        "^".repeat(caret_len),
        RESET.as_str()
    )
}

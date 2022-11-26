use crate::lex::Span;

#[macro_export]
macro_rules! color {
    ($color:ident) => {
        format!(
            "{}{}",
            termion::color::Fg(termion::color::$color),
            termion::style::Bold
        )
    };
}

#[macro_export]
macro_rules! reset {
    () => {
        format!(
            "{}{}",
            termion::color::Fg(termion::color::Reset),
            termion::style::Reset
        )
    };
}

#[derive(Debug)]
pub enum Error {
    Io(String),
    Lex(Span, String),
    Parse(Span, String),
    Type(Span, String, Vec<Note>),
    Main(Option<Span>, String, Vec<Note>),
    Import(Option<Span>, String, Vec<Note>),
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
                print!("{}io error:{} {}", color!(Red), reset!(), msg);
            }
            Error::Lex(span, msg) => {
                println!("{}lex error:{} {msg}", color!(Red), reset!());
                print_span(*span);
            }
            Error::Parse(span, msg) => {
                println!("{}parse error:{} {msg}", color!(Red), reset!());
                print_span(*span);
            }
            Error::Type(span, msg, notes) => {
                println!("{}type error:{} {msg}", color!(Red), reset!());
                print_span(*span);
                print_notes(notes);
            }
            Error::Main(span, msg, notes) => {
                if let Some(span) = span {
                    println!("{}entry error:{} {msg}", color!(Red), reset!());
                    print_span(*span);
                } else {
                    print!("{}entry error:{} {msg}", color!(Red), reset!());
                }
                print_notes(notes);
            }
            Error::Import(span, msg, notes) => {
                if let Some(span) = span {
                    println!("{}import error:{} {msg}", color!(Red), reset!());
                    print_span(*span);
                } else {
                    print!("{}import error:{} {msg}", color!(Red), reset!());
                }
                print_notes(notes);
            }
        }
    }

    pub fn println(&self) {
        self.print();
        println!();
    }

    pub fn insert_note(&mut self, note: Note) {
        match self {
            Error::Type(_, _, notes) => notes.insert(0, note),
            _ => panic!(),
        }
    }
}

fn print_span(span: Span) {
    let line_num = format!("{}", span.line + 1);
    let space = " ".repeat(line_num.len());
    println!(
        "{}{}-->{} {}",
        space,
        color!(Blue),
        reset!(),
        span.location()
    );

    println!("{} {}|", space, color!(Blue));
    println!("{} | {}{}", line_num, reset!(), span.line());
    println!(
        "{} {}| {}",
        space,
        color!(Blue),
        get_carets(span.line_prefix(), span.text)
    );
}

fn get_carets(prefix: &str, text: &str) -> String {
    let white_len = prefix.chars().count();
    let caret_len = std::cmp::max(text.chars().count(), 1);
    format!(
        "{}{}{}{}",
        " ".repeat(white_len),
        color!(Red),
        "^".repeat(caret_len),
        reset!()
    )
}

fn print_notes(notes: &[Note]) {
    for Note { span, msg } in notes {
        if let Some(span) = span {
            println!("\n{}note:{} {msg}", color!(Blue), reset!());
            print_span(*span);
        } else {
            print!("\n{}note:{} {msg}", color!(Blue), reset!());
        }
    }
}

use crate::ast::Name;
use crate::lex::Span;
use crate::typecheck::{GSignature, GType, Kind, Primitive};

use lazy_static::lazy_static;
use std::collections::HashMap;

macro_rules! sig {
    ($op:expr, $($params:expr)* => $($returns:expr)*) => {
        GSignature::new(
            Name {
                name: $op,
                span: Span::empty()
            },
            Kind::Builtin,
            vec![$($params),*], vec![$($returns),*]
        )
    };
}

macro_rules! prim {
    ($($var:ident => $name:ident),*) => {
        $(const $name: GType = GType::Primitive(Primitive::$var);)*
    };
}

macro_rules! arith {
    ($op:expr $(, $($sigs:expr),*)?) => {(
        $op,
        vec![
            sig!($op, BYTE BYTE => BYTE),
            sig!($op, BYTE INT => INT),
            sig!($op, BYTE FLOAT => FLOAT),
            sig!($op, INT BYTE => INT),
            sig!($op, INT INT => INT),
            sig!($op, INT FLOAT => FLOAT),
            sig!($op, FLOAT BYTE => FLOAT),
            sig!($op, FLOAT INT => FLOAT),
            sig!($op, FLOAT FLOAT => FLOAT),
            $($($sigs),*)?
        ]
    )};
}

macro_rules! bit {
    ($op:expr $(, $($sigs:expr),*)?) => {(
        $op,
        vec![
            sig!($op, BYTE BYTE => BYTE),
            sig!($op, INT INT => INT),
            sig!($op, BOOL BOOL => BOOL),
            $($($sigs),*)?
        ]
    )};
}

macro_rules! eq {
    ($op:expr) => {(
        $op,
        vec![
            sig!($op, BYTE BYTE => BOOL),
            sig!($op, INT INT => BOOL),
            sig!($op, FLOAT FLOAT => BOOL),
            sig!($op, BOOL BOOL => BOOL),
        ]
    )};
}

macro_rules! cmp {
    ($op:expr) => {(
        $op,
        vec![
            sig!($op, BYTE BYTE => BOOL),
            sig!($op, INT INT => BOOL),
            sig!($op, FLOAT FLOAT => BOOL),
            sig!($op, genp!() genp!()  => BOOL),
        ]
    )};
}

prim! {Byte => BYTE, Int => INT, Float => FLOAT, Bool => BOOL}

macro_rules! ptr {
    ($ty:expr) => {
        GType::Pointer(Box::new($ty))
    };
}

macro_rules! gen {
    ($id:expr) => {
        GType::Generic($id)
    };
}

macro_rules! genp {
    () => {
        ptr!(gen!(0))
    };
}

/*

struct string {
    *byte bytes
    int len
    int capacity
}

fn string *byte int int -> string
fn .bytes string -> *byte
fn .len string -> int
fn .capacity string -> int
fn ..bytes string -> string *byte
fn ..len string -> string int
fn ..capacity string -> string int
fn to_string_ptr [T] *T -> *string
fn size_of string -> int

//////////////////////////////////////////

import string

fn hello_world {
    "hello world" new_string
    & '!' push
    .bytes putln
}
*/
lazy_static! {
    pub static ref BUILTINS: HashMap<&'static str, Vec<GSignature>> = HashMap::from([
        arith!(
            "+",
            sig!("+", genp!() INT => genp!()),
            sig!("+", INT genp!() => genp!())
        ),
        arith!(
            "-",
            sig!("-", genp!() INT => genp!()),
            sig!("-", INT genp!() => genp!())
        ),
        arith!("*", sig!("*", genp!() => gen!(0))),
        arith!("/"),
        arith!("%"),
        bit!("&"),
        bit!("^"),
        bit!("|"),
        eq!("=="),
        eq!("!="),
        cmp!("<"),
        cmp!("<="),
        cmp!(">"),
        cmp!(">="),
        (
            "!",
            vec![
                sig!("!", BOOL => BOOL),
                sig!("!", BYTE => BYTE),
                sig!("!", INT => INT),
                sig!("!", FLOAT => FLOAT),
            ]
        ),
        (".", vec![sig!(".", gen!(0) => gen!(0) gen!(0))]),
        ("~", vec![sig!("~", gen!(0) => )]),
        ("@", vec![sig!("@", gen!(0) => gen!(0) genp!())]),
        ("_", vec![sig!("_", => )]),
        (
            "to_byte",
            vec![sig!("to_byte", INT => BYTE), sig!("to_byte", FLOAT => BYTE)]
        ),
        (
            "to_int",
            vec![
                sig!("to_int", FLOAT => INT),
                sig!("to_int", BYTE => INT),
                sig!("to_int", genp!() => INT),
            ]
        ),
        (
            "to_float",
            vec![
                sig!("to_float", INT => FLOAT),
                sig!("to_float", BYTE => FLOAT),
            ]
        ),
        (
            "to_byte_ptr",
            vec![sig!(
                "to_byte_ptr",
                genp!() => ptr!(BYTE)
            )]
        ),
        (
            "to_int_ptr",
            vec![sig!(
                "to_int_ptr",
                genp!() => ptr!(INT)
            )]
        ),
        (
            "to_float_ptr",
            vec![sig!(
                "to_float_ptr",
                genp!() => ptr!(FLOAT)
            )]
        ),
        (
            "to_bool_ptr",
            vec![sig!(
                "to_bool_ptr",
                genp!() => ptr!(BOOL)
            )]
        ),
        (
            "size_of",
            vec![
                sig!("size_of", INT => INT),
                sig!("size_of", FLOAT => INT),
                sig!("size_of", BYTE => INT),
                sig!("size_of", BOOL => INT),
                sig!("size_of", genp!() => INT),
            ]
        ),
        (
            "neg",
            vec![sig!("neg", INT => INT), sig!("neg", FLOAT => FLOAT)]
        ),
        (
            "put",
            vec![
                sig!("put", INT =>),
                sig!("put", FLOAT =>),
                sig!("put", BYTE =>),
                sig!("put", BOOL =>),
            ]
        ),
        (
            "putln",
            vec![
                sig!("putln", INT =>),
                sig!("putln", FLOAT =>),
                sig!("putln", BYTE =>),
                sig!("putln", BOOL =>),
            ]
        ),
        ("puts", vec![sig!("puts", ptr!(BYTE) =>)]),
        ("putlns", vec![sig!("putlns", ptr!(BYTE) =>)]),
        ("putc", vec![sig!("putc", INT =>)]),
        ("putlnc", vec![sig!("putlnc", INT =>)]),
    ]);
}

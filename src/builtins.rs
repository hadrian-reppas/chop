use crate::ast::Name;
use crate::lex::Span;
use crate::typecheck::{Primitive, Signature, Type};

use lazy_static::lazy_static;
use std::collections::HashMap;

macro_rules! sig {
    ($op:expr, $($params:expr)* => $($returns:expr)*) => {
        Signature::new(
            Name {
                name: $op,
                span: Span::empty()
            },
            true,
            vec![$($params),*], vec![$($returns),*]
        )
    };
}

macro_rules! prim {
    ($($var:ident => $name:ident),*) => {
        $(const $name: Type = Type::Primitive(Primitive::$var);)*
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
        ]
    )};
}

prim! {Byte => BYTE, Int => INT, Float => FLOAT, Bool => BOOL}

// TODO: add casts
// fn to_int byte -> int
// fn to_float int -> float
// ...
//
// fn to_byte_ptr [T] *T -> *byte
// fn to_int_ptr [T] *T -> *int
// ...
// (every struct def adds fn to_{struct name}_ptr [T] *T -> *{struct name})

// TOOD: add pointer arithmetic
// fn ptr_to_int [T] *T -> int
// fn + [T] *T int -> *T
// fn - [T] *T int -> *T
// fn == [T] *T *T -> bool
// fn != [T] *T *T -> bool
// fn <= [T] *T *T -> bool
// fn < [T] *T *T -> bool
// fn >= [T] *T *T -> bool
// fn > [T] *T *T -> bool

/*
struct String {
    *byte bytes
    int len
    int capacity
}

fn String *byte int int -> string
fn .bytes string -> *byte
fn .len string -> int
fn .capacity string -> int
fn ..bytes string -> string *byte
fn ..len string -> string int
fn ..capacity string -> string int
fn to_string_ptr [T] *T -> *string

import string

fn hello_world {
    "hello world" new_string
    & '!' push
    .bytes putln
}
*/
lazy_static! {
    pub static ref BUILTINS: HashMap<&'static str, Vec<Signature>> = HashMap::from([
        arith!("+"),
        arith!("-"),
        arith!(
            "*",
            sig!("*", Type::Pointer(Box::new(Type::Generic(0))) => Type::Generic(0))
        ),
        arith!("/"),
        arith!("%"),
        bit!(
            "&",
            sig!("&", Type::Generic(0) => Type::Generic(0) Type::Pointer(Box::new(Type::Generic(0))))
        ),
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
                sig!("!", FLOAT => FLOAT)
            ]
        ),
        (
            ".",
            vec![sig!(".", Type::Generic(0) => Type::Generic(0) Type::Generic(0))]
        ),
        ("~", vec![sig!("~", Type::Generic(0) => )]),
    ]);
}

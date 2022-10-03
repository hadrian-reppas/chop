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
            sig!($op, genp!(0) genp!(0) => BOOL),
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
            sig!($op, genp!(0) genp!(1)  => BOOL),
        ]
    )};
}

macro_rules! shift {
    ($op:expr) => {(
        $op,
        vec![
            sig!($op, BYTE BYTE => BYTE),
            sig!($op, BYTE INT => BYTE),
            sig!($op, INT INT => INT),
            sig!($op, INT BYTE => INT),
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
    ($id:expr) => {
        ptr!(gen!($id))
    };
}

lazy_static! {
    pub static ref BUILTINS: HashMap<&'static str, Vec<GSignature>> = HashMap::from([
        arith!(
            "+",
            sig!("+", genp!(0) INT => genp!(0)),
            sig!("+", INT genp!(0) => genp!(0))
        ),
        arith!("-", sig!("-", genp!(0) INT => genp!(0))),
        arith!("*", sig!("*", genp!(0) => gen!(0))),
        arith!("/"),
        arith!("%"),
        bit!("&"),
        bit!("^"),
        bit!("|"),
        shift!("<<"),
        shift!(">>"),
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
            ]
        ),
        (".", vec![sig!(".", gen!(0) => gen!(0) gen!(0))]),
        ("~", vec![sig!("~", gen!(0) => )]),
        ("@", vec![sig!("@", gen!(0) => gen!(0) genp!(0))]),
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
                sig!("to_int", genp!(0) => INT),
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
            vec![
                sig!("to_byte_ptr", genp!(0) => ptr!(BYTE)),
                sig!("to_byte_ptr", INT => ptr!(BYTE)),
            ]
        ),
        (
            "to_int_ptr",
            vec![
                sig!("to_int_ptr", genp!(0) => ptr!(INT)),
                sig!("to_int_ptr", INT => ptr!(INT)),
            ]
        ),
        (
            "to_float_ptr",
            vec![
                sig!("to_float_ptr", genp!(0) => ptr!(FLOAT)),
                sig!("to_float_ptr", INT => ptr!(FLOAT)),
            ]
        ),
        (
            "to_bool_ptr",
            vec![
                sig!("to_bool_ptr", genp!(0) => ptr!(BOOL)),
                sig!("to_bool_ptr", INT => ptr!(BOOL)),
            ]
        ),
        (
            "size_of",
            vec![
                sig!("size_of", INT => INT),
                sig!("size_of", FLOAT => INT),
                sig!("size_of", BYTE => INT),
                sig!("size_of", BOOL => INT),
                sig!("size_of", genp!(0) => INT),
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
                sig!("put", genp!(0) =>),
            ]
        ),
        (
            "putln",
            vec![
                sig!("putln", INT =>),
                sig!("putln", FLOAT =>),
                sig!("putln", BYTE =>),
                sig!("putln", BOOL =>),
                sig!("putln", genp!(0) =>),
            ]
        ),
        ("puts", vec![sig!("puts", ptr!(BYTE) =>)]),
        ("putlns", vec![sig!("putlns", ptr!(BYTE) =>)]),
        ("putc", vec![sig!("putc", INT =>)]),
        ("putlnc", vec![sig!("putlnc", INT =>)]),
        ("ln", vec![sig!("ln", =>)]),
        ("panic", vec![sig!("panic", =>)]),
        ("assert", vec![sig!("assert", BOOL =>)]),
        ("store", vec![sig!("store", genp!(0) gen!(0) =>)]),
        ("exit", vec![sig!("exit", INT =>)]),
        ("alloc", vec![sig!("alloc", INT => ptr!(BYTE))]),
        ("zalloc", vec![sig!("zalloc", INT => ptr!(BYTE))]),
        ("realloc", vec![sig!("realloc", genp!(0) INT => genp!(0))]),
        ("free", vec![sig!("free", genp!(0) =>)]),
        ("copy", vec![sig!("copy", genp!(0) genp!(0) INT =>)]),
        ("pow", vec![sig!("pow", FLOAT FLOAT => FLOAT)]),
        ("random", vec![sig!("random", => FLOAT)]),
        ("randint", vec![sig!("randint", INT => INT)]),
        ("strcmp", vec![sig!("strcmp", ptr!(BYTE) ptr!(BYTE) => INT)]),
        ("streq", vec![sig!("streq", ptr!(BYTE) ptr!(BYTE) => BOOL)]),
        ("strcpy", vec![sig!("strcpy", ptr!(BYTE) ptr!(BYTE) =>)]),
        ("strlen", vec![sig!("strlen", ptr!(BYTE) => INT)]),
        ("alloc_int", vec![sig!("alloc_int", => ptr!(INT))]),
        ("zalloc_int", vec![sig!("zalloc_int", => ptr!(INT))]),
        ("alloc_int_arr", vec![sig!("alloc_int_arr", INT => ptr!(INT))]),
        ("zalloc_int_arr", vec![sig!("zalloc_int_arr", INT => ptr!(INT))]),
        ("alloc_float", vec![sig!("alloc_float", => ptr!(FLOAT))]),
        ("zalloc_float", vec![sig!("zalloc_float", => ptr!(FLOAT))]),
        ("alloc_float_arr", vec![sig!("alloc_float_arr", INT => ptr!(FLOAT))]),
        ("zalloc_float_arr", vec![sig!("zalloc_float_arr", INT => ptr!(FLOAT))]),
        ("alloc_byte", vec![sig!("alloc_byte", => ptr!(BYTE))]),
        ("zalloc_byte", vec![sig!("zalloc_byte", => ptr!(BYTE))]),
        ("alloc_byte_arr", vec![sig!("alloc_byte_arr", INT => ptr!(BYTE))]),
        ("zalloc_byte_arr", vec![sig!("zalloc_byte_arr", INT => ptr!(BYTE))]),
        ("alloc_bool", vec![sig!("alloc_bool", => ptr!(BOOL))]),
        ("zalloc_bool", vec![sig!("zalloc_bool", => ptr!(BOOL))]),
        ("alloc_bool_arr", vec![sig!("alloc_bool_arr", INT => ptr!(BOOL))]),
        ("zalloc_bool_arr", vec![sig!("zalloc_bool_arr", INT => ptr!(BOOL))]),
        ("DEBUG_STACK", vec![sig!("DEBUG_STACK", =>)]),
        // TODO: files
        // TODO: stdin
    ]);
}

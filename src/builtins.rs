use lazy_static::lazy_static;

use crate::types::GType;

macro_rules! sig {
    ($($params:expr)* => $($returns:expr)*) => {
        (vec![$($params),*], vec![$($returns),*], 0)
    };
    ($($params:expr)* => $($returns:expr)*, $count:expr) => {
        (vec![$($params),*], vec![$($returns),*], $count)
    };
}

macro_rules! prim {
    ($($var:ident => $name:ident),*) => {
        $(const $name: GType = GType::$var(0);)*
    };
}

macro_rules! arith {
    ($op:expr $(, $($sigs:expr),*)?) => {(
        $op,
        vec![
            sig!(BYTE BYTE => BYTE),
            sig!(BYTE INT => INT),
            sig!(BYTE FLOAT => FLOAT),
            sig!(INT BYTE => INT),
            sig!(INT INT => INT),
            sig!(INT FLOAT => FLOAT),
            sig!(FLOAT BYTE => FLOAT),
            sig!(FLOAT INT => FLOAT),
            sig!(FLOAT FLOAT => FLOAT),
            $($($sigs),*)?
        ]
    )};
}

macro_rules! bit {
    ($op:expr) => {(
        $op,
        vec![
            sig!(BYTE BYTE => BYTE),
            sig!(INT INT => INT),
            sig!(BOOL BOOL => BOOL),
        ]
    )};
}

macro_rules! eq {
    ($op:expr) => {(
        $op,
        vec![
            sig!(BYTE BYTE => BOOL),
            sig!(BYTE INT => BOOL),
            sig!(BYTE FLOAT => BOOL),
            sig!(INT BYTE => BOOL),
            sig!(INT INT => BOOL),
            sig!(INT FLOAT => BOOL),
            sig!(FLOAT BYTE => BOOL),
            sig!(FLOAT INT => BOOL),
            sig!(FLOAT FLOAT => BOOL),
            sig!(BOOL BOOL => BOOL),
            sig!(genp!(0) genp!(0) => BOOL, 1),
        ]
    )};
}

macro_rules! cmp {
    ($op:expr) => {(
        $op,
        vec![
            sig!(BYTE BYTE => BOOL),
            sig!(BYTE INT => BOOL),
            sig!(BYTE FLOAT => BOOL),
            sig!(INT BYTE => BOOL),
            sig!(INT INT => BOOL),
            sig!(INT FLOAT => BOOL),
            sig!(FLOAT BYTE => BOOL),
            sig!(FLOAT INT => BOOL),
            sig!(FLOAT FLOAT => BOOL),
            sig!(genp!(0) genp!(0) => BOOL, 1),
        ]
    )};
}

macro_rules! shift {
    ($op:expr) => {(
        $op,
        vec![
            sig!(BYTE BYTE => BYTE),
            sig!(BYTE INT => BYTE),
            sig!(INT BYTE => INT),
            sig!(INT INT => INT),
        ]
    )};
}

prim! {Byte => BYTE, Int => INT, Float => FLOAT, Bool => BOOL}

macro_rules! ptr {
    ($ty:expr) => {
        $ty.ref_n(1)
    };
}

macro_rules! gen {
    ($index:expr) => {
        GType::Generic(0, $index)
    };
}

macro_rules! genp {
    ($index:expr) => {
        GType::Generic(1, $index)
    };
}

type Pair = (Vec<GType>, Vec<GType>, usize);
type NamePairs = (&'static str, Vec<Pair>);
pub type Builtins = Vec<NamePairs>;

lazy_static! {
    pub static ref BUILTINS: Builtins = Vec::from([
        arith!(
            "+",
            sig!(genp!(0) INT => genp!(0), 1),
            sig!(INT genp!(0) => genp!(0), 1)
        ),
        arith!(
            "-",
            sig!(genp!(0) INT => genp!(0), 1),
            sig!(genp!(0) genp!(0) => INT, 1)
        ),
        arith!("*"),
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
            vec![sig!(BOOL => BOOL), sig!(BYTE => BYTE), sig!(INT => INT),]
        ),
        ("neg", vec![sig!(INT => INT), sig!(FLOAT => FLOAT),]),
        (".", vec![sig!(gen!(0) => gen!(0) gen!(0), 1)]),
        ("~", vec![sig!(gen!(0) =>, 1)]),
        ("@", vec![sig!(gen!(0) => gen!(0) genp!(0), 1)]),
        ("to_byte", vec![sig!(INT => BYTE), sig!(FLOAT => BYTE),]),
        (
            "to_int",
            vec![
                sig!(FLOAT => INT),
                sig!(BYTE => INT),
                sig!(genp!(0) => INT, 1),
            ]
        ),
        ("to_float", vec![sig!(INT => FLOAT), sig!(BYTE => FLOAT),]),
        (
            "put",
            vec![sig!(INT =>), sig!(FLOAT =>), sig!(BYTE =>), sig!(BOOL =>),]
        ),
        (
            "putln",
            vec![sig!(INT =>), sig!(FLOAT =>), sig!(BYTE =>), sig!(BOOL =>),]
        ),
        ("puts", vec![sig!(ptr!(BYTE) =>)]),
        ("putlns", vec![sig!(ptr!(BYTE) =>)]),
        ("putc", vec![sig!(INT =>)]),
        ("putlnc", vec![sig!(INT =>)]),
        ("putp", vec![sig!(genp!(0) =>, 1)]),
        ("putlnp", vec![sig!(genp!(0) =>, 1)]),
        ("ln", vec![sig!(=>)]),
        ("read", vec![sig!(genp!(0) => gen!(0), 1)]),
        ("write", vec![sig!(genp!(0) gen!(0) =>, 1)]),
        ("exit", vec![sig!(INT =>)]),
        ("realloc", vec![sig!(genp!(0) INT => genp!(0), 1)]),
        ("free", vec![sig!(genp!(0) =>, 1)]),
        ("copy", vec![sig!(genp!(0) genp!(0) INT =>, 1)]),
        ("pow", vec![sig!(FLOAT FLOAT => FLOAT)]),
        ("random", vec![sig!(=> FLOAT)]),
        ("strcmp", vec![sig!(ptr!(BYTE) ptr!(BYTE) => INT)]),
        ("streq", vec![sig!(ptr!(BYTE) ptr!(BYTE) => BOOL)]),
        ("strcpy", vec![sig!(ptr!(BYTE) ptr!(BYTE) =>)]),
        ("strlen", vec![sig!(ptr!(BYTE) => INT)]),
        ("read_file", vec![sig!(ptr!(BYTE) => ptr!(BYTE))]),
        ("write_to_file", vec![sig!(ptr!(BYTE) ptr!(BYTE) => BOOL)]),
        ("append_to_file", vec![sig!(ptr!(BYTE) ptr!(BYTE) => BOOL)]),
        ("time", vec![sig!(=> FLOAT)]),
        ("stdin", vec![sig!(=> ptr!(BYTE))]),
        ("DEBUG_STACK", vec![sig!(=>)]),
    ]);
}

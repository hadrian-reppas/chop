use crate::ast::Item;
use crate::ast::{self, *};
use crate::error::Error;
use crate::lex::Span;

use lazy_static::lazy_static;
use std::collections::{hash_map::Entry, HashMap};
use std::fmt;

#[derive(PartialEq, Clone)]
enum Type {
    Primitive(Primitive),
    Pointer(Box<Type>),
    Generic(usize),
    Custom(&'static str),
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Primitive(Primitive::Int) => write!(f, "int"),
            Type::Primitive(Primitive::Float) => write!(f, "float"),
            Type::Primitive(Primitive::Byte) => write!(f, "byte"),
            Type::Primitive(Primitive::Bool) => write!(f, "bool"),
            Type::Pointer(ty) => write!(f, "*{ty:?}"),
            Type::Generic(n) => write!(f, "T{}", n),
            Type::Custom(name) => write!(f, "{}", name),
        }
    }
}

impl Type {
    fn new(ty: ast::Type) -> Type {
        match ty {
            ast::Type::Normal(name) => match name.name {
                "byte" => Type::Primitive(Primitive::Byte),
                "int" => Type::Primitive(Primitive::Int),
                "float" => Type::Primitive(Primitive::Float),
                "bool" => Type::Primitive(Primitive::Bool),
                name => Type::Custom(name),
            },
            ast::Type::Pointer(name, depth) => {
                let mut ty = Type::new(ast::Type::Normal(name));
                for _ in 0..depth {
                    ty = Type::Pointer(Box::new(ty));
                }
                ty
            }
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
enum Primitive {
    Int,   // i32
    Float, // f32
    Byte,  // u8
    Bool,  // i1
}

macro_rules! sig {
    ($($params:expr)* => $($returns:expr)*) => {
        Signature::new(vec![$($params),*], vec![$($returns),*])
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
    ($op:expr $(, $($sigs:expr),*)?) => {(
        $op,
        vec![
            sig!(BYTE BYTE => BYTE),
            sig!(INT INT => INT),
            sig!(BOOL BOOL => BOOL),
            $($($sigs),*)?
        ]
    )};
}

macro_rules! eq {
    ($op:expr) => {(
        $op,
        vec![
            sig!(BYTE BYTE => BOOL),
            sig!(INT INT => BOOL),
            sig!(FLOAT FLOAT => BOOL),
            sig!(BOOL BOOL => BOOL),
        ]
    )};
}

macro_rules! cmp {
    ($op:expr) => {(
        $op,
        vec![
            sig!(BYTE BYTE => BOOL),
            sig!(INT INT => BOOL),
            sig!(FLOAT FLOAT => BOOL),
        ]
    )};
}

prim! {Byte => BYTE, Int => INT, Float => FLOAT, Bool => BOOL}

lazy_static! {
    static ref BUILTINS: HashMap<&'static str, Vec<Signature>> = HashMap::from([
        arith!("+"),
        arith!("-"),
        arith!(
            "*",
            sig!(Type::Pointer(Box::new(Type::Generic(0))) => Type::Generic(0))
        ),
        arith!("/"),
        arith!("%"),
        bit!(
            "&",
            sig!(Type::Generic(0) => Type::Generic(0) Type::Pointer(Box::new(Type::Generic(0))))
        ),
        bit!("^"),
        bit!("|"),
        eq!("=="),
        eq!("!="),
        cmp!("<"),
        cmp!("<="),
        cmp!(">"),
        cmp!(">="),
    ]);
}

#[derive(Clone)]
struct Signature {
    params: Vec<Type>,
    returns: Vec<Type>,
}

impl fmt::Debug for Signature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} -> {:?}", self.params, self.returns)
    }
}

impl Signature {
    fn new(params: Vec<Type>, returns: Vec<Type>) -> Signature {
        Signature { params, returns }
    }
}

struct Context {
    signatures: HashMap<&'static str, Vec<Signature>>,
    stack: Vec<Type>,
}

impl Context {
    fn new() -> Context {
        Context {
            signatures: BUILTINS.clone(),
            stack: Vec::new(),
        }
    }

    fn init_signatures(&mut self, unit: &[Item]) -> Result<(), Error> {
        for item in unit {
            let (name, signature) = get_signature(item);
            self.insert_signature(name, signature)?;
        }
        Ok(())
    }

    fn insert_signature(&mut self, name: Name, signature: Signature) -> Result<(), Error> {
        match self.signatures.entry(name.name) {
            Entry::Occupied(o) => {
                for existing in o.get() {
                    if conflict(&existing.params, &signature.params) {
                        // TODO: keep track of span in self.signatures
                        // to include it in this error message
                        return Err(Error::Type(
                            name.span,
                            "signature conflicts with a previous definition".to_string(),
                        ));
                    }
                }
                Ok(())
            }
            Entry::Vacant(e) => {
                e.insert(vec![signature]);
                Ok(())
            }
        }
    }

    fn update_stack(&mut self, name: Name) -> Result<(), Error> {
        todo!()
    }

    fn check_item(&mut self, item: &Item) -> Result<(), Error> {
        match item {
            Item::Function {
                name,
                params,
                returns,
                body,
                rbrace_span,
                ..
            } => {
                for stmt in body {
                    self.check_stmt(stmt)?;
                }
                Ok(())
            }
        }
    }

    fn check_stmt(&mut self, stmt: &Stmt) -> Result<(), Error> {
        todo!()
    }

    /*
    fn define_structs(&mut self, unit: &Vec<Item>) -> Result<(), Error> {
        todo!()
    }
    */
}

fn get_signature(item: &Item) -> (Name, Signature) {
    match item {
        Item::Function {
            name,
            params,
            returns,
            ..
        } => (
            *name,
            Signature {
                params: params.iter().copied().map(Type::new).collect(),
                returns: returns.iter().copied().map(Type::new).collect(),
            },
        ),
    }
}

fn is_suffix(stack: &[Type], params: &[Type]) -> bool {
    params.len() <= stack.len()
        && stack
            .iter()
            .rev()
            .zip(params.iter().rev())
            .all(|(s, p)| s == p)
}

fn conflict(params_a: &[Type], params_b: &[Type]) -> bool {
    params_a
        .iter()
        .rev()
        .zip(params_b.iter().rev())
        .all(|(a, b)| a == b)
}

pub fn typecheck(unit: &Vec<Item>) -> Result<(), Error> {
    let mut context = Context::new();

    context.init_signatures(unit)?;

    println!("{:#?}", context.signatures);

    Ok(()) // TODO
}

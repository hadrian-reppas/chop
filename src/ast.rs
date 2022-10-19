use std::fmt;

use crate::lex::{Span, Token};
use std::hash::{Hash, Hasher};

#[derive(Clone, Copy)]
pub struct Name {
    pub name: &'static str,
    pub span: Span,
}

impl fmt::Debug for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl PartialEq for Name {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for Name {}

impl Hash for Name {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl Name {
    pub fn is_normal(&self) -> bool {
        let c = self.name.chars().next().unwrap();
        crate::lex::is_normal_start(c)
    }

    pub fn is_ptr(&self) -> bool {
        self.name.chars().all(|c| c == '*')
    }

    pub fn new(name: &'static str) -> Name {
        Name {
            name,
            span: Span::empty(),
        }
    }
}

pub type Unit = Vec<Item>;

#[allow(clippy::large_enum_variant)]
pub enum Item {
    Function {
        name: Name,
        generics: Option<Generics>,
        params: Vec<PType>,
        returns: Vec<PType>,
        body: Vec<Stmt>,

        fn_span: Span,
        arrow_span: Option<Span>,
        lbrace_span: Span,
        rbrace_span: Span,
    },
    Struct {
        name: Name,
        generics: Option<Generics>,
        fields: Vec<Field>,

        struct_span: Span,
        lbrace_span: Span,
        rbrace_span: Span,
    },
    Global {
        name: Name,
        ty: PType,
        definition: Option<Definition>,

        global_span: Span,
        colon_span: Span,
    },
    Import {
        path: Vec<Name>,

        import_span: Span,
        colon_spans: Vec<Span>,
    },
}

impl fmt::Debug for Item {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Item::Function {
                name,
                params,
                returns,
                body,
                ..
            } => write!(f, "Function({name:?}, {params:?}, {returns:?}, {body:?})"),
            Item::Struct {
                name,
                generics,
                fields,
                ..
            } => write!(f, "Struct({name:?}, {generics:?}, {fields:?})"),
            Item::Global {
                name,
                ty,
                definition,
                ..
            } => write!(f, "Global({name:?}, {ty:?}, {definition:?})"),
            Item::Import { path, .. } => write!(f, "Import({:?})", path),
        }
    }
}

pub struct Definition {
    pub group: Vec<Op>,

    pub lbrace_span: Span,
    pub rbrace_span: Span,
}

impl fmt::Debug for Definition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Definition({:?})", self.group)
    }
}

pub struct Field {
    pub name: Name,
    pub ty: PType,

    pub colon_span: Span,
}

impl fmt::Debug for Field {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}: {:?}", self.name.name, self.ty)
    }
}

#[derive(Clone)]
pub struct Generics {
    pub names: Vec<Name>,

    pub lbrack_span: Span,
    pub rbrack_span: Span,
}

impl fmt::Debug for Generics {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Generics({:?})", self.names)
    }
}

#[derive(Clone)]
pub struct PType {
    pub stars: Option<Name>,
    pub name: Name,
    pub generics: Option<TypeGenerics>,
}

impl fmt::Debug for PType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "PType({:?}, {:?}, {:?})",
            self.name, self.stars, self.generics
        )
    }
}

#[derive(Clone)]
pub struct TypeGenerics {
    pub types: Vec<PType>,

    pub lbrack_span: Span,
    pub rbrack_span: Span,
}

impl fmt::Debug for TypeGenerics {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "TypeGenerics({:?})", self.types)
    }
}

pub type Group = Vec<Op>;

#[allow(clippy::large_enum_variant)]
#[derive(Clone)]
pub enum Stmt {
    Group(Group, Span),
    If {
        test: Group,
        body: Vec<Stmt>,

        if_span: Span,
        lbrace_span: Span,
        rbrace_span: Span,

        else_part: Option<ElsePart>,
        is_else_if: bool,
    },
    While {
        test: Group,
        body: Vec<Stmt>,

        while_span: Span,
        lbrace_span: Span,
        rbrace_span: Span,
    },
    For {
        low: Group,
        high: Group,
        body: Vec<Stmt>,

        for_span: Span,
        to_span: Span,
        lbrace_span: Span,
        rbrace_span: Span,
    },
    Let {
        names: Vec<Name>,
        body: Vec<Stmt>,

        let_span: Span,
        lbrace_span: Span,
        rbrace_span: Span,
    },
}

impl fmt::Debug for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::Group(group, _) => write!(f, "Group({group:?})"),
            Stmt::If {
                test,
                body,
                else_part,
                ..
            } => write!(f, "If({test:?}, {body:?}, {else_part:?})"),
            Stmt::While { test, body, .. } => write!(f, "While({test:?}, {body:?})"),
            Stmt::For {
                low, high, body, ..
            } => write!(f, "For({low:?}, {high:?}, {body:?})"),
            Stmt::Let { names, body, .. } => write!(f, "Let({names:?}, {body:?})"),
        }
    }
}

impl Stmt {
    pub fn brace_spans(&self) -> (Span, Span) {
        match self {
            Stmt::If {
                lbrace_span,
                rbrace_span,
                ..
            } => (*lbrace_span, *rbrace_span),
            _ => panic!(),
        }
    }

    pub fn set_else_if(&mut self) {
        match self {
            Stmt::If { is_else_if, .. } => *is_else_if = true,
            _ => panic!(),
        }
    }
}

#[derive(Clone)]
pub struct ElsePart {
    pub body: Vec<Stmt>,

    pub else_span: Span,
    pub lbrace_span: Span,
    pub rbrace_span: Span,
}

impl fmt::Debug for ElsePart {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Else({:?})", self.body)
    }
}

#[derive(Clone)]
pub enum Op {
    Int(i64, Span),
    Float(f64, Span),
    Bool(bool, Span),
    Char(char, Span),
    String(String, Span),
    Name(Name),
    SizeOf(PType, Span, Span, Span),
    Alloc(PType, Span, Span, Span),
    Zalloc(PType, Span, Span, Span),
    AllocArr(PType, Span, Span, Span),
    ZallocArr(PType, Span, Span, Span),
    CastTo(PType, Span, Span, Span),
    Expr(Expr, Span),
}

impl fmt::Debug for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op::Int(i, _) => write!(f, "Int({i})"),
            Op::Float(x, _) => write!(f, "Float({x})"),
            Op::Bool(b, _) => write!(f, "Bool({b})"),
            Op::Char(c, _) => write!(f, "Char({c:?})"),
            Op::String(s, _) => write!(f, "String({s:?})"),
            Op::Name(n) => write!(f, "Name({n:?})"),
            Op::SizeOf(ty, _, _, _) => write!(f, "SizeOf({ty:?})"),
            Op::Alloc(ty, _, _, _) => write!(f, "Alloc({ty:?})"),
            Op::Zalloc(ty, _, _, _) => write!(f, "Zalloc({ty:?})"),
            Op::AllocArr(ty, _, _, _) => write!(f, "AllocArr({ty:?})"),
            Op::ZallocArr(ty, _, _, _) => write!(f, "ZallocArr({ty:?})"),
            Op::CastTo(ty, _, _, _) => write!(f, "CastTo({ty:?})"),
            Op::Expr(e, _) => write!(f, "{e:?}"),
        }
    }
}

impl Op {
    pub fn from_token(token: Token) -> Op {
        match token {
            Token::Int(i, span) => Op::Int(i, span),
            Token::Float(f, span) => Op::Float(f, span),
            Token::Bool(b, span) => Op::Bool(b, span),
            Token::Char(c, span) => Op::Char(c, span),
            Token::String(s, span) => Op::String(s, span),
            Token::Name(span) => Op::Name(Name {
                name: span.text,
                span,
            }),
            _ => panic!(),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Op::Int(_, span) => *span,
            Op::Float(_, span) => *span,
            Op::Bool(_, span) => *span,
            Op::Char(_, span) => *span,
            Op::String(_, span) => *span,
            Op::Name(name) => name.span,
            Op::SizeOf(_, span, _, _) => *span,
            Op::Alloc(_, span, _, _) => *span,
            Op::Zalloc(_, span, _, _) => *span,
            Op::AllocArr(_, span, _, _) => *span,
            Op::ZallocArr(_, span, _, _) => *span,
            Op::CastTo(_, span, _, _) => *span,
            Op::Expr(_, span) => *span,
        }
    }
}

#[derive(Clone)]
pub enum Expr {
    Int(i64, Span),
    Float(f64, Span),
    Bool(bool, Span),
    Char(char, Span),
    Name(Name),
    Add(Box<Expr>, Box<Expr>, Span),
    And(Box<Expr>, Box<Expr>, Span),
    Divide(Box<Expr>, Box<Expr>, Span),
    Equal(Box<Expr>, Box<Expr>, Span),
    GreaterEqual(Box<Expr>, Box<Expr>, Span),
    GreaterThan(Box<Expr>, Box<Expr>, Span),
    LessEqual(Box<Expr>, Box<Expr>, Span),
    LessThan(Box<Expr>, Box<Expr>, Span),
    Modulo(Box<Expr>, Box<Expr>, Span),
    Multiply(Box<Expr>, Box<Expr>, Span),
    NotEqual(Box<Expr>, Box<Expr>, Span),
    Or(Box<Expr>, Box<Expr>, Span),
    Subtract(Box<Expr>, Box<Expr>, Span),
    Xor(Box<Expr>, Box<Expr>, Span),
    LShift(Box<Expr>, Box<Expr>, Span),
    RShift(Box<Expr>, Box<Expr>, Span),
    Not(Box<Expr>, Span),
    Negate(Box<Expr>, Span),
    Deref(Box<Expr>, Span),
    Group(Vec<Op>, Span),
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Int(i, _) => write!(f, "Int({i})"),
            Expr::Float(x, _) => write!(f, "Float({x})"),
            Expr::Bool(b, _) => write!(f, "Bool({b})"),
            Expr::Char(c, _) => write!(f, "Char({c:?})"),
            Expr::Name(name) => write!(f, "Name({name:?})"),
            Expr::Add(left, right, _) => write!(f, "Add({left:?}, {right:?})"),
            Expr::And(left, right, _) => write!(f, "And({left:?}, {right:?})"),
            Expr::Divide(left, right, _) => write!(f, "Divide({left:?}, {right:?})"),
            Expr::Equal(left, right, _) => write!(f, "Equal({left:?}, {right:?})"),
            Expr::GreaterEqual(left, right, _) => write!(f, "GreaterEqual({left:?}, {right:?})"),
            Expr::GreaterThan(left, right, _) => write!(f, "GreaterThan({left:?}, {right:?})"),
            Expr::LessEqual(left, right, _) => write!(f, "LessEqual({left:?}, {right:?})"),
            Expr::LessThan(left, right, _) => write!(f, "LessThan({left:?}, {right:?})"),
            Expr::Modulo(left, right, _) => write!(f, "Modulo({left:?}, {right:?})"),
            Expr::Multiply(left, right, _) => write!(f, "Multiply({left:?}, {right:?})"),
            Expr::NotEqual(left, right, _) => write!(f, "NotEqual({left:?}, {right:?})"),
            Expr::Or(left, right, _) => write!(f, "Or({left:?}, {right:?})"),
            Expr::Subtract(left, right, _) => write!(f, "Subtract({left:?}, {right:?})"),
            Expr::Xor(left, right, _) => write!(f, "Xor({left:?}, {right:?})"),
            Expr::LShift(left, right, _) => write!(f, "LShift({left:?}, {right:?})"),
            Expr::RShift(left, right, _) => write!(f, "RShift({left:?}, {right:?})"),
            Expr::Not(expr, _) => write!(f, "Not({expr:?})"),
            Expr::Negate(expr, _) => write!(f, "Negate({expr:?})"),
            Expr::Deref(expr, _) => write!(f, "Deref({expr:?})"),
            Expr::Group(group, _) => write!(f, "Group({group:?})"),
        }
    }
}

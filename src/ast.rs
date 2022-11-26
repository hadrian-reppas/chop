use crate::lex::{Span, Token};

#[derive(Clone, Copy)]
pub struct Name {
    pub name: &'static str,
    pub span: Span,
}

impl Name {
    pub fn is_normal(&self) -> bool {
        let c = self.name.chars().next().unwrap();
        crate::lex::is_normal_start(c)
    }

    pub fn is_ptr(&self) -> bool {
        self.name.chars().all(|c| c == '*')
    }
}

#[derive(Clone, Copy)]
pub enum QualifiedName {
    Straight(Name),
    Qualified(Name, Name),
}

impl QualifiedName {
    pub fn is_just(&self, name: &str) -> bool {
        match self {
            QualifiedName::Straight(n) => n.name == name,
            QualifiedName::Qualified(_, _) => false,
        }
    }

    pub fn span(&self) -> Span {
        self.name().span
    }

    pub fn name(&self) -> Name {
        match self {
            QualifiedName::Straight(name) => *name,
            QualifiedName::Qualified(_, name) => *name,
        }
    }
}

#[allow(clippy::large_enum_variant)]
pub enum Item {
    Function {
        name: Name,
        generics: Option<Generics>,
        params: Vec<PType>,
        returns: Vec<PType>,
        body: Vec<Stmt>,
        rbrace_span: Span,
    },
    Struct {
        name: Name,
        generics: Option<Generics>,
        fields: Vec<Field>,
    },
    Global {
        name: Name,
        ty: PType,
        definition: Option<Definition>,
    },
    Import {
        path: Vec<Name>,
        names: Vec<Name>,
    },
    ImportModule {
        path: Vec<Name>,
    },
    ImportStruct {
        path: Vec<Name>,
        names: Vec<Name>,
    },
}

pub struct Definition {
    pub group: Vec<Op>,
    pub rbrace_span: Span,
}

pub struct Field {
    pub name: Name,
    pub ty: PType,
}

#[derive(Clone)]
pub struct Generics {
    pub names: Vec<Name>,
}

#[derive(Clone)]
pub struct PType {
    pub stars: Option<Name>,
    pub name: QualifiedName,
    pub generics: Option<TypeGenerics>,
}

#[derive(Clone)]
pub struct TypeGenerics {
    pub types: Vec<PType>,
}

pub type Group = Vec<Op>;

#[allow(clippy::large_enum_variant)]
#[derive(Clone)]
pub enum Stmt {
    Group(Group, Span),
    If {
        test: Group,
        body: Vec<Stmt>,
        lbrace_span: Span,
        rbrace_span: Span,
        else_part: Option<ElsePart>,
        is_else_if: bool,
    },
    While {
        test: Group,
        body: Vec<Stmt>,
        lbrace_span: Span,
        rbrace_span: Span,
    },
    For {
        low: Group,
        high: Group,
        body: Vec<Stmt>,
        to_span: Span,
        lbrace_span: Span,
        rbrace_span: Span,
    },
    Let {
        names: Vec<Name>,
        body: Vec<Stmt>,
        let_span: Span,
    },
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
    pub lbrace_span: Span,
    pub rbrace_span: Span,
}

#[derive(Clone)]
pub enum Op {
    Int(i64, Span),
    Float(f64, Span),
    Bool(bool, Span),
    Char(char, Span),
    String(String, Span),
    Byte(u8, Span),
    Name(QualifiedName),
    SizeOf(PType, Span, Span, Span),
    Alloc(PType, Span, Span, Span),
    Zalloc(PType, Span, Span, Span),
    AllocArr(PType, Span, Span, Span),
    ZallocArr(PType, Span, Span, Span),
    CastTo(PType, Span, Span, Span),
    Assert(Span),
    Abort(Span, Vec<PType>),
    Expr(Expr, Span),
}

impl Op {
    pub fn from_token(token: Token) -> Op {
        match token {
            Token::Int(i, span) => Op::Int(i, span),
            Token::Float(f, span) => Op::Float(f, span),
            Token::Bool(b, span) => Op::Bool(b, span),
            Token::Char(c, span) => Op::Char(c, span),
            Token::String(s, span) => Op::String(s, span),
            Token::Byte(b, span) => Op::Byte(b, span),
            _ => panic!(),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Op::Int(_, span)
            | Op::Float(_, span)
            | Op::Bool(_, span)
            | Op::Char(_, span)
            | Op::String(_, span)
            | Op::Byte(_, span)
            | Op::SizeOf(_, span, _, _)
            | Op::Alloc(_, span, _, _)
            | Op::Zalloc(_, span, _, _)
            | Op::AllocArr(_, span, _, _)
            | Op::ZallocArr(_, span, _, _)
            | Op::CastTo(_, span, _, _)
            | Op::Assert(span)
            | Op::Abort(span, _)
            | Op::Expr(_, span) => *span,
            Op::Name(qname) => qname.span(),
        }
    }
}

#[derive(Clone)]
pub enum Expr {
    Int(i64, Span),
    Float(f64, Span),
    Bool(bool, Span),
    Char(char, Span),
    String(String, Span),
    Byte(u8, Span),
    Name(QualifiedName),
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
    Call(QualifiedName, Vec<Expr>),
}

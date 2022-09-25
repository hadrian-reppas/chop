use crate::ast::Expr;
use crate::ast::{Group, Name};
use crate::error::Error;
use crate::lex::{Span, Token, Tokens};
use crate::parse::parse_group;

use std::iter::Peekable;

enum Tok {
    Int(i128, Span),
    Float(f64, Span),
    Bool(bool, Span),
    Char(char, Span),
    Var(Name),
    Group(Group, Span),

    LParen(Span),
    RParen(Span),

    Add(Span),
    And(Span),
    Divide(Span),
    Equal(Span),
    GreaterEqual(Span),
    GreaterThan(Span),
    LessEqual(Span),
    LessThan(Span),
    Modulo(Span),
    Multiply(Span),
    NotEqual(Span),
    Or(Span),
    Subtract(Span),
    Xor(Span),

    Not(Span),
}

impl Tok {
    fn span(&self) -> Span {
        match self {
            Tok::Int(_, span) => *span,
            Tok::Float(_, span) => *span,
            Tok::Bool(_, span) => *span,
            Tok::Char(_, span) => *span,
            Tok::Var(name) => name.span,
            Tok::LParen(span) => *span,
            Tok::RParen(span) => *span,
            Tok::Add(span) => *span,
            Tok::And(span) => *span,
            Tok::Divide(span) => *span,
            Tok::Equal(span) => *span,
            Tok::GreaterEqual(span) => *span,
            Tok::GreaterThan(span) => *span,
            Tok::LessEqual(span) => *span,
            Tok::LessThan(span) => *span,
            Tok::Modulo(span) => *span,
            Tok::Multiply(span) => *span,
            Tok::NotEqual(span) => *span,
            Tok::Or(span) => *span,
            Tok::Subtract(span) => *span,
            Tok::Xor(span) => *span,
            Tok::Not(span) => *span,
            Tok::Group(_, span) => *span,
        }
    }
}

pub fn parse(tokens: &mut Tokens) -> Result<(Expr, Span), Error> {
    let mut tok_vec = convert(tokens)?;
    let rparen = tok_vec.pop().unwrap();
    tok_vec.remove(0);
    let mut tokens = tok_vec.into_iter().peekable();
    Ok((pratt_parse(&mut tokens, 0)?, rparen.span()))
}

fn convert(tokens: &mut Tokens) -> Result<Vec<Tok>, Error> {
    let mut toks = vec![Tok::LParen(tokens.next()?.span())];
    let mut depth = 1;
    while depth > 0 {
        match tokens.next()? {
            Token::Int(i, span) => toks.push(Tok::Int(i, span)),
            Token::Float(f, span) => toks.push(Tok::Float(f, span)),
            Token::Bool(b, span) => toks.push(Tok::Bool(b, span)),
            Token::Char(c, span) => toks.push(Tok::Char(c, span)),
            Token::Name(span) => {
                toks.push(match span.text {
                    "+" => Tok::Add(span),
                    "&" => Tok::And(span),
                    "/" => Tok::Divide(span),
                    "==" => Tok::Equal(span),
                    ">=" => Tok::GreaterEqual(span),
                    ">" => Tok::GreaterThan(span),
                    "<=" => Tok::LessEqual(span),
                    "<" => Tok::LessThan(span),
                    "%" => Tok::Modulo(span),
                    "*" => Tok::Multiply(span),
                    "!=" => Tok::NotEqual(span),
                    "|" => Tok::Or(span),
                    "-" => Tok::Subtract(span),
                    "^" => Tok::Xor(span),
                    "!" => Tok::Not(span),
                    name => Tok::Var(Name { name, span }),
                });
            }
            Token::LParen(span) => {
                toks.push(Tok::LParen(span));
                depth += 1;
            }
            Token::RParen(span) => {
                toks.push(Tok::RParen(span));
                depth -= 1;
            }
            Token::LBrack(_) => {
                let group = parse_group(tokens)?;
                if !tokens.peek().is_rbrack() {
                    return Err(Error::Parse(
                        tokens.peek().span(),
                        "expected ']'".to_string(),
                    ));
                }
                let span = tokens.next()?.span();
                toks.push(Tok::Group(group, span));
            }
            token => {
                return Err(Error::Parse(
                    token.span(),
                    "unexpected token in expression".to_string(),
                ));
            }
        }
    }
    Ok(toks)
}

const OR_BP: usize = 6;
const XOR_BP: usize = 8;
const AND_BP: usize = 10;
const EQUAL_BP: usize = 14;
const NOT_EQUAL_BP: usize = 14;
const GREATER_EQUAL_BP: usize = 16;
const GREATER_THAN_BP: usize = 16;
const LESS_EQUAL_BP: usize = 16;
const LESS_THAN_BP: usize = 16;
const ADD_BP: usize = 24;
const SUBTRACT_BP: usize = 24;
const DIVIDE_BP: usize = 30;
const MODULO_BP: usize = 30;
const MULTIPLY_BP: usize = 30;
const NEGATE_BP: usize = 40;
const NOT_BP: usize = 40;

fn pratt_parse(tokens: &mut Peekable<impl Iterator<Item = Tok>>, bp: usize) -> Result<Expr, Error> {
    let mut lhs = match tokens.next().unwrap() {
        Tok::Int(i, span) => Expr::Int(i, span),
        Tok::Float(f, span) => Expr::Float(f, span),
        Tok::Bool(b, span) => Expr::Bool(b, span),
        Tok::Char(c, span) => Expr::Char(c, span),
        Tok::Var(name) => Expr::Name(name),
        Tok::Group(group, span) => Expr::Group(group, span),
        Tok::Subtract(span) => Expr::Negate(Box::new(pratt_parse(tokens, NEGATE_BP)?), span),
        Tok::Not(span) => Expr::Not(Box::new(pratt_parse(tokens, NOT_BP)?), span),
        Tok::LParen(span) => {
            let lhs = pratt_parse(tokens, 0)?;
            if !matches!(tokens.peek(), Some(Tok::RParen(_))) {
                return Err(Error::Parse(span, "expected ')'".to_string()));
            }
            lhs
        }
        token => return Err(Error::Parse(token.span(), "unexpected token".to_string())),
    };

    macro_rules! expr {
        ($name:ident, $bp:expr, $token:expr) => {{
            if $bp < bp {
                break;
            }
            let token = $token.clone();
            tokens.next();
            Expr::$name(Box::new(lhs), Box::new(pratt_parse(tokens, $bp)?), token)
        }};
    }

    loop {
        lhs = match tokens.peek() {
            Some(Tok::Add(token)) => expr!(Add, ADD_BP, token),
            Some(Tok::And(token)) => expr!(And, AND_BP, token),
            Some(Tok::Divide(token)) => expr!(Divide, DIVIDE_BP, token),
            Some(Tok::Equal(token)) => expr!(Equal, EQUAL_BP, token),
            Some(Tok::GreaterEqual(token)) => expr!(GreaterEqual, GREATER_EQUAL_BP, token),
            Some(Tok::GreaterThan(token)) => expr!(GreaterThan, GREATER_THAN_BP, token),
            Some(Tok::LessEqual(token)) => expr!(LessEqual, LESS_EQUAL_BP, token),
            Some(Tok::LessThan(token)) => expr!(LessThan, LESS_THAN_BP, token),
            Some(Tok::Modulo(token)) => expr!(Modulo, MODULO_BP, token),
            Some(Tok::Multiply(token)) => expr!(Multiply, MULTIPLY_BP, token),
            Some(Tok::NotEqual(token)) => expr!(NotEqual, NOT_EQUAL_BP, token),
            Some(Tok::Or(token)) => expr!(Or, OR_BP, token),
            Some(Tok::Subtract(token)) => expr!(Subtract, SUBTRACT_BP, token),
            Some(Tok::Xor(token)) => expr!(Xor, XOR_BP, token),
            Some(Tok::RParen(_)) => break,
            Some(token) => return Err(Error::Parse(token.span(), "unexpected token".to_string())),
            None => break,
        }
    }

    Ok(lhs)
}

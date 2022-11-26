use std::iter::Peekable;

use crate::ast::{Expr, Group, QualifiedName};
use crate::error::Error;
use crate::lex::{Span, Token, Tokens};
use crate::parse::{parse_colon, parse_group, parse_name};

const OR_BP: usize = 6;
const XOR_BP: usize = 8;
const AND_BP: usize = 10;
const EQUAL_BP: usize = 14;
const NOT_EQUAL_BP: usize = 14;
const GREATER_EQUAL_BP: usize = 16;
const GREATER_THAN_BP: usize = 16;
const LESS_EQUAL_BP: usize = 16;
const LESS_THAN_BP: usize = 16;
const SHIFT_BP: usize = 18;
const ADD_BP: usize = 24;
const SUBTRACT_BP: usize = 24;
const DIVIDE_BP: usize = 30;
const MODULO_BP: usize = 30;
const MULTIPLY_BP: usize = 30;
const NEGATE_BP: usize = 40;
const NOT_BP: usize = 40;
const DEREF_BP: usize = 40;

enum Tok {
    Int(i64, Span),
    Float(f64, Span),
    Bool(bool, Span),
    Char(char, Span),
    Byte(u8, Span),
    String(String, Span),
    Name(QualifiedName),
    Group(Group, Span),

    LParen(Span),
    RParen(Span),
    Comma(Span),

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
    LShift(Span),
    RShift(Span),

    Not(Span),
}

impl Tok {
    fn span(&self) -> Span {
        match self {
            Tok::Int(_, span)
            | Tok::Float(_, span)
            | Tok::Bool(_, span)
            | Tok::Char(_, span)
            | Tok::Byte(_, span)
            | Tok::String(_, span)
            | Tok::LParen(span)
            | Tok::RParen(span)
            | Tok::Comma(span)
            | Tok::Add(span)
            | Tok::And(span)
            | Tok::Divide(span)
            | Tok::Equal(span)
            | Tok::GreaterEqual(span)
            | Tok::GreaterThan(span)
            | Tok::LessEqual(span)
            | Tok::LessThan(span)
            | Tok::Modulo(span)
            | Tok::Multiply(span)
            | Tok::NotEqual(span)
            | Tok::Or(span)
            | Tok::Subtract(span)
            | Tok::Xor(span)
            | Tok::LShift(span)
            | Tok::RShift(span)
            | Tok::Not(span)
            | Tok::Group(_, span) => *span,
            Tok::Name(qname) => qname.span(),
        }
    }
}

pub fn parse(tokens: &mut Tokens) -> Result<(Expr, Span), Error> {
    let mut tok_vec = convert(tokens)?;
    let rparen_span = tok_vec.pop().unwrap().span();
    tok_vec.remove(0);
    let mut tokens = tok_vec.into_iter().peekable();
    let expr = pratt_parse(&mut tokens, 0, rparen_span)?;
    if let Some(extra) = tokens.next() {
        Err(Error::Parse(extra.span(), "unexpected token".to_string()))
    } else {
        Ok((expr, rparen_span))
    }
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
            Token::Byte(b, span) => toks.push(Tok::Byte(b, span)),
            Token::String(s, span) => toks.push(Tok::String(s, span)),
            Token::Name(span) => {
                let first = span.into();
                let tok = if tokens.peek().is_colon() {
                    tokens.next()?.span();
                    parse_colon(tokens)?;
                    let second = parse_name(tokens, false)?;
                    Tok::Name(QualifiedName::Qualified(first, second))
                } else {
                    match first.name {
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
                        "<<" => Tok::LShift(span),
                        ">>" => Tok::RShift(span),
                        _ => Tok::Name(QualifiedName::Straight(first)),
                    }
                };
                toks.push(tok);
            }
            Token::LParen(span) => {
                toks.push(Tok::LParen(span));
                depth += 1;
            }
            Token::RParen(span) => {
                toks.push(Tok::RParen(span));
                depth -= 1;
            }
            Token::Comma(span) => toks.push(Tok::Comma(span)),
            Token::LBrack(_) => {
                let group = parse_group(tokens, false)?;
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

fn pratt_parse(
    tokens: &mut Peekable<impl Iterator<Item = Tok>>,
    bp: usize,
    rparen_span: Span,
) -> Result<Expr, Error> {
    let mut lhs = match tokens.next() {
        Some(Tok::Int(i, span)) => Expr::Int(i, span),
        Some(Tok::Float(f, span)) => Expr::Float(f, span),
        Some(Tok::Bool(b, span)) => Expr::Bool(b, span),
        Some(Tok::Char(c, span)) => Expr::Char(c, span),
        Some(Tok::Byte(b, span)) => Expr::Byte(b, span),
        Some(Tok::String(s, span)) => Expr::String(s, span),
        Some(Tok::Name(name)) => Expr::Name(name),
        Some(Tok::Group(group, span)) => Expr::Group(group, span),
        Some(Tok::Subtract(span)) => {
            Expr::Negate(Box::new(pratt_parse(tokens, NEGATE_BP, rparen_span)?), span)
        }
        Some(Tok::Not(span)) => {
            Expr::Not(Box::new(pratt_parse(tokens, NOT_BP, rparen_span)?), span)
        }
        Some(Tok::Multiply(span)) => {
            Expr::Deref(Box::new(pratt_parse(tokens, DEREF_BP, rparen_span)?), span)
        }
        Some(Tok::LParen(span)) => {
            let lhs = pratt_parse(tokens, 0, rparen_span)?;
            if !matches!(tokens.peek(), Some(Tok::RParen(_))) {
                return Err(Error::Parse(span, "expected ')'".to_string()));
            }
            tokens.next();
            lhs
        }
        Some(token) => return Err(Error::Parse(token.span(), "unexpected token".to_string())),
        None => {
            return Err(Error::Parse(
                rparen_span,
                "expected an expression".to_string(),
            ))
        }
    };

    macro_rules! expr {
        ($name:ident, $bp:expr) => {{
            if $bp < bp {
                break;
            }
            let span = tokens.next().unwrap().span();
            Expr::$name(
                Box::new(lhs),
                Box::new(pratt_parse(tokens, $bp, rparen_span)?),
                span,
            )
        }};
    }

    loop {
        lhs = match tokens.peek() {
            Some(Tok::Add(_)) => expr!(Add, ADD_BP),
            Some(Tok::And(_)) => expr!(And, AND_BP),
            Some(Tok::Divide(_)) => expr!(Divide, DIVIDE_BP),
            Some(Tok::Equal(_)) => expr!(Equal, EQUAL_BP),
            Some(Tok::GreaterEqual(_)) => expr!(GreaterEqual, GREATER_EQUAL_BP),
            Some(Tok::GreaterThan(_)) => expr!(GreaterThan, GREATER_THAN_BP),
            Some(Tok::LessEqual(_)) => expr!(LessEqual, LESS_EQUAL_BP),
            Some(Tok::LessThan(_)) => expr!(LessThan, LESS_THAN_BP),
            Some(Tok::Modulo(_)) => expr!(Modulo, MODULO_BP),
            Some(Tok::Multiply(_)) => expr!(Multiply, MULTIPLY_BP),
            Some(Tok::NotEqual(_)) => expr!(NotEqual, NOT_EQUAL_BP),
            Some(Tok::Or(_)) => expr!(Or, OR_BP),
            Some(Tok::Subtract(_)) => expr!(Subtract, SUBTRACT_BP),
            Some(Tok::Xor(_)) => expr!(Xor, XOR_BP),
            Some(Tok::LShift(_)) => expr!(LShift, SHIFT_BP),
            Some(Tok::RShift(_)) => expr!(RShift, SHIFT_BP),
            Some(Tok::LParen(_)) => {
                let span = tokens.next().unwrap().span();
                let name = if let Expr::Name(name) = lhs {
                    name
                } else {
                    return Err(Error::Parse(span, "unexpected token".to_string()));
                };
                let mut args = Vec::new();
                if matches!(tokens.peek(), Some(Tok::RParen(_))) {
                    tokens.next();
                    Expr::Call(name, args)
                } else {
                    loop {
                        args.push(pratt_parse(tokens, 0, rparen_span)?);
                        if matches!(tokens.peek(), Some(Tok::RParen(_))) {
                            tokens.next();
                            break Expr::Call(name, args);
                        } else if matches!(tokens.peek(), Some(Tok::Comma(_))) {
                            tokens.next();
                        } else if let Some(token) = tokens.peek() {
                            return Err(Error::Parse(token.span(), "unexpected token".to_string()));
                        } else {
                            return Err(Error::Parse(
                                rparen_span,
                                "expected ',' or ')'".to_string(),
                            ));
                        }
                    }
                }
            }
            Some(Tok::RParen(_)) | Some(Tok::Comma(_)) | None => break,
            Some(token) => return Err(Error::Parse(token.span(), "unexpected token".to_string())),
        }
    }

    Ok(lhs)
}

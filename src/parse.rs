use crate::ast::*;
use crate::error::Error;
use crate::expr;
use crate::lex::{Span, Token, Tokens};

pub fn parse(mut tokens: Tokens) -> Result<Unit, Error> {
    let mut items = Vec::new();
    loop {
        match tokens.peek() {
            Token::Fn(_) => items.push(parse_fn(&mut tokens)?),
            Token::Eof(_) => return Ok(items),
            token => return Err(Error::Parse(token.span(), "unexpected token".to_string())),
        }
    }
}

pub fn parse_file(path: &str) -> Result<Unit, Error> {
    let tokens = Tokens::from_file(path)?;
    parse(tokens)
}

// TODO: parse pointer types
fn parse_fn(tokens: &mut Tokens) -> Result<Item, Error> {
    let fn_span = tokens.next()?.span();
    if !tokens.peek().is_name() {
        return Err(Error::Parse(
            tokens.peek().span(),
            "expected name token".to_string(),
        ));
    }
    let name_span = tokens.next()?.span();
    let name = Name {
        name: name_span.text,
        span: name_span,
    };

    let mut params = Vec::new();
    let mut returns = Vec::new();
    let mut arrow_span = None;

    while tokens.peek().is_name() {
        let span = tokens.next()?.span();
        params.push(Name {
            name: span.text,
            span,
        });
    }

    let lbrace_span = if tokens.peek().is_arrow() {
        arrow_span = Some(tokens.next()?.span());
        while tokens.peek().is_name() {
            let span = tokens.next()?.span();
            returns.push(Name {
                name: span.text,
                span,
            });
        }
        if tokens.peek().is_lbrace() {
            tokens.next()?.span()
        } else {
            return Err(Error::Parse(
                tokens.peek().span(),
                "expected name or '{'".to_string(),
            ));
        }
    } else if tokens.peek().is_lbrace() {
        tokens.next()?.span()
    } else {
        return Err(Error::Parse(
            tokens.peek().span(),
            "expected name, '->' or '{'".to_string(),
        ));
    };

    let params = combine_pointers(params)?;
    let returns = combine_pointers(returns)?;

    let (body, rbrace_span) = parse_body(tokens)?;

    Ok(Item::Function {
        name,
        params,
        returns,
        body,
        fn_span,
        arrow_span,
        lbrace_span,
        rbrace_span,
    })
}

#[allow(clippy::type_complexity)]
fn combine_pointers(names: Vec<Name>) -> Result<Vec<Type>, Error> {
    let mut types = Vec::new();
    let mut i = 0;
    while i < names.len() {
        let name = names[i];
        if is_ptr(name.name) {
            if i + 1 < names.len() && is_normal(names[i + 1].name) {
                types.push(Type::Pointer(names[i + 1], name.name.len()));
                i += 1;
            } else {
                return Err(Error::Parse(name.span, "invalid pointer type".to_string()));
            }
        } else if is_normal(name.name) {
            types.push(Type::Normal(name));
        } else {
            return Err(Error::Parse(
                name.span,
                "types must have normal names".to_string(),
            ));
        }
        i += 1;
    }
    Ok(types)
}

fn is_normal(name: &str) -> bool {
    let c = name.chars().next().unwrap();
    crate::lex::is_normal_start(c)
}

fn is_ptr(name: &str) -> bool {
    name.chars().all(|c| c == '*')
}

fn parse_body(tokens: &mut Tokens) -> Result<(Vec<Stmt>, Span), Error> {
    let mut body = Vec::new();

    while !tokens.peek().is_rbrace() {
        body.push(parse_stmt(tokens)?);
    }

    Ok((body, tokens.next()?.span()))
}

fn parse_stmt(tokens: &mut Tokens) -> Result<Stmt, Error> {
    if tokens.peek().is_standalone() || tokens.peek().is_lparen() {
        let group = parse_group(tokens)?;
        let span = group[0].span();
        Ok(Stmt::Group(group, span))
    } else if tokens.peek().is_if() {
        parse_if(tokens)
    } else if tokens.peek().is_while() {
        parse_while(tokens)
    } else if tokens.peek().is_for() {
        parse_for(tokens)
    } else if tokens.peek().is_let() {
        parse_let(tokens)
    } else {
        Err(Error::Parse(
            tokens.peek().span(),
            "unexpected token".to_string(),
        ))
    }
}

pub fn parse_group(tokens: &mut Tokens) -> Result<Vec<Op>, Error> {
    let mut group = Vec::new();
    while tokens.peek().is_standalone() || tokens.peek().is_lparen() {
        if tokens.peek().is_standalone() {
            group.push(Op::from_token(tokens.next()?));
        } else {
            let (expr, span) = expr::parse(tokens)?;
            group.push(Op::Expr(expr, span));
        }
    }

    if group.is_empty() {
        Err(Error::Parse(
            tokens.peek().span(),
            "expected a group".to_string(),
        ))
    } else {
        Ok(group)
    }
}

fn parse_test(tokens: &mut Tokens) -> Result<(Option<Vec<Op>>, Span), Error> {
    if tokens.peek().is_lbrace() {
        Ok((None, tokens.next()?.span()))
    } else {
        let group = parse_group(tokens)?;
        if !tokens.peek().is_lbrace() {
            return Err(Error::Parse(
                tokens.peek().span(),
                "expected '{'".to_string(),
            ));
        }
        Ok((Some(group), tokens.next()?.span()))
    }
}

fn parse_if(tokens: &mut Tokens) -> Result<Stmt, Error> {
    let if_span = tokens.next()?.span();

    let (test, lbrace_span) = parse_test(tokens)?;
    let (body, rbrace_span) = parse_body(tokens)?;

    if tokens.peek().is_else() {
        let else_span = tokens.next()?.span();
        if tokens.peek().is_lbrace() {
            let else_lbrace_span = tokens.next()?.span();
            let (else_body, else_rbrace_span) = parse_body(tokens)?;
            Ok(Stmt::If {
                test,
                body,
                if_span,
                lbrace_span,
                rbrace_span,
                else_part: Some(ElsePart {
                    body: else_body,
                    else_span,
                    lbrace_span: else_lbrace_span,
                    rbrace_span: else_rbrace_span,
                }),
            })
        } else {
            Err(Error::Parse(
                tokens.peek().span(),
                "expected '{'".to_string(),
            ))
        }
    } else {
        Ok(Stmt::If {
            test,
            body,
            if_span,
            lbrace_span,
            rbrace_span,
            else_part: None,
        })
    }
}

fn parse_while(tokens: &mut Tokens) -> Result<Stmt, Error> {
    let while_span = tokens.next()?.span();

    let (test, lbrace_span) = parse_test(tokens)?;
    let (body, rbrace_span) = parse_body(tokens)?;

    Ok(Stmt::While {
        test,
        body,
        while_span,
        lbrace_span,
        rbrace_span,
    })
}

fn parse_for(tokens: &mut Tokens) -> Result<Stmt, Error> {
    let for_span = tokens.next()?.span();

    let low = parse_group(tokens)?;

    if !tokens.peek().is_to() {
        return Err(Error::Parse(
            tokens.peek().span(),
            "expected 'to'".to_string(),
        ));
    }

    let to_span = tokens.next()?.span();

    let (high, lbrace_span) = parse_test(tokens)?;
    let high = if let Some(high) = high {
        high
    } else {
        return Err(Error::Parse(lbrace_span, "expected a group".to_string()));
    };

    let (body, rbrace_span) = parse_body(tokens)?;

    Ok(Stmt::For {
        low,
        high,
        body,
        for_span,
        to_span,
        lbrace_span,
        rbrace_span,
    })
}

fn parse_let(tokens: &mut Tokens) -> Result<Stmt, Error> {
    let let_span = tokens.next()?.span();
    let mut names = Vec::new();

    while tokens.peek().is_name() {
        let span = tokens.next()?.span();
        names.push(Name {
            name: span.text,
            span,
        });
    }

    if names.is_empty() {
        return Err(Error::Parse(
            tokens.peek().span(),
            "expected a name".to_string(),
        ));
    } else if !tokens.peek().is_lbrace() {
        return Err(Error::Parse(
            tokens.peek().span(),
            "expected '{'".to_string(),
        ));
    }

    let lbrace_span = tokens.next()?.span();
    let (body, rbrace_span) = parse_body(tokens)?;

    Ok(Stmt::Let {
        names,
        body,
        let_span,
        lbrace_span,
        rbrace_span,
    })
}

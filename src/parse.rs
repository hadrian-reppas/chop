use crate::ast::*;
use crate::error::Error;
use crate::expr;
use crate::lex::{Span, Token, Tokens};

use lazy_static::lazy_static;
use std::collections::HashSet;

lazy_static! {
    static ref RESERVED_TYPES: HashSet<&'static str> =
        HashSet::from(["byte", "int", "float", "bool", "ptr", "_"]);
}

pub fn parse(mut tokens: Tokens) -> Result<Unit, Error> {
    let mut items = Vec::new();
    loop {
        match tokens.peek() {
            Token::Fn(_) => items.push(parse_fn(&mut tokens)?),
            Token::Struct(_) => items.push(parse_struct(&mut tokens)?),
            Token::Global(_) => items.push(parse_global(&mut tokens)?),
            Token::Import(_) => items.push(parse_import(&mut tokens)?),
            Token::Eof(_) => return Ok(items),
            token => return Err(Error::Parse(token.span(), "unexpected token".to_string())),
        }
    }
}

pub fn parse_file(path: &str) -> Result<Unit, Error> {
    let tokens = Tokens::from_file(path)?;
    parse(tokens)
}

fn parse_name(tokens: &mut Tokens) -> Result<Name, Error> {
    if tokens.peek().is_name() {
        let span = tokens.next()?.span();
        Ok(Name {
            name: span.text,
            span,
        })
    } else {
        Err(Error::Parse(
            tokens.peek().span(),
            "expected name token".to_string(),
        ))
    }
}

fn parse_fn(tokens: &mut Tokens) -> Result<Item, Error> {
    let fn_span = tokens.next()?.span();
    let name = parse_name(tokens)?;

    let generics = parse_generics(tokens)?;

    let mut params = Vec::new();
    let mut returns = Vec::new();
    let mut arrow_span = None;

    while tokens.peek().is_name() {
        params.push(parse_type(tokens)?);
    }

    let lbrace_span = if tokens.peek().is_arrow() {
        arrow_span = Some(tokens.next()?.span());
        while tokens.peek().is_name() {
            returns.push(parse_type(tokens)?);
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

    let (body, rbrace_span) = parse_body(tokens)?;

    Ok(Item::Function {
        name,
        generics,
        params,
        returns,
        body,
        fn_span,
        arrow_span,
        lbrace_span,
        rbrace_span,
    })
}

fn parse_generics(tokens: &mut Tokens) -> Result<Option<Generics>, Error> {
    if tokens.peek().is_lbrack() {
        let lbrack_span = tokens.next()?.span();

        let mut names = Vec::new();
        while tokens.peek().is_name() {
            let generic = parse_name(tokens)?;
            if !generic.is_normal() {
                return Err(Error::Parse(
                    generic.span,
                    "generics must have normal names".to_string(),
                ));
            } else if RESERVED_TYPES.contains(generic.name) {
                return Err(Error::Parse(
                    generic.span,
                    format!("'{}' is a reserved type name", generic.name),
                ));
            }
            names.push(generic);
        }

        if !tokens.peek().is_rbrack() {
            return Err(Error::Parse(
                tokens.peek().span(),
                "expected ']'".to_string(),
            ));
        }

        let rbrack_span = tokens.next()?.span();

        Ok(Some(Generics {
            names,
            lbrack_span,
            rbrack_span,
        }))
    } else {
        Ok(None)
    }
}

fn parse_struct(tokens: &mut Tokens) -> Result<Item, Error> {
    let struct_span = tokens.next()?.span();
    let name = parse_name(tokens)?;
    if !name.is_normal() {
        return Err(Error::Parse(
            name.span,
            "structs must have normal names".to_string(),
        ));
    }
    let generics = parse_generics(tokens)?;
    if !tokens.peek().is_lbrace() {
        return Err(Error::Parse(
            tokens.peek().span(),
            "expected '{'".to_string(),
        ));
    }
    let lbrace_span = tokens.next()?.span();

    let mut fields = Vec::new();
    while !tokens.peek().is_rbrace() {
        let name = parse_name(tokens)?;
        if !tokens.peek().is_colon() {
            return Err(Error::Parse(
                tokens.peek().span(),
                "expected ':'".to_string(),
            ));
        }
        let colon_span = tokens.next()?.span();
        let ty = parse_type(tokens)?;
        fields.push(Field {
            name,
            ty,
            colon_span,
        });
    }

    let rbrace_span = tokens.next()?.span();

    Ok(Item::Struct {
        name,
        generics,
        fields,
        struct_span,
        lbrace_span,
        rbrace_span,
    })
}

fn parse_global(tokens: &mut Tokens) -> Result<Item, Error> {
    let global_span = tokens.next()?.span();
    let name = parse_name(tokens)?;
    if !tokens.peek().is_colon() {
        return Err(Error::Parse(
            tokens.peek().span(),
            "expected ':'".to_string(),
        ));
    }
    let colon_span = tokens.next()?.span();
    let ty = parse_type(tokens)?;

    let definition = if tokens.peek().is_lbrace() {
        let lbrace_span = tokens.next()?.span();
        let group = parse_group(tokens)?;
        if !tokens.peek().is_rbrace() {
            return Err(Error::Parse(
                tokens.peek().span(),
                "expected '}'".to_string(),
            ));
        }
        let rbrace_span = tokens.next()?.span();
        Some(Definition {
            group,
            lbrace_span,
            rbrace_span,
        })
    } else {
        None
    };

    Ok(Item::Global {
        name,
        ty,
        definition,
        global_span,
        colon_span,
    })
}

fn parse_import(tokens: &mut Tokens) -> Result<Item, Error> {
    let import_span = tokens.next()?.span();
    let mut path = vec![parse_name(tokens)?];
    let mut colon_spans = Vec::new();
    while tokens.peek().is_colon() {
        colon_spans.push(tokens.next()?.span());
        path.push(parse_name(tokens)?);
    }
    Ok(Item::Import {
        path,
        import_span,
        colon_spans,
    })
}

fn parse_type(tokens: &mut Tokens) -> Result<PType, Error> {
    let first = parse_name(tokens)?;
    if first.is_normal() {
        let generics = parse_type_generics(tokens)?;
        Ok(PType {
            stars: None,
            name: first,
            generics,
        })
    } else if first.is_ptr() {
        let second = parse_name(tokens)?;
        if second.is_normal() {
            let generics = parse_type_generics(tokens)?;
            Ok(PType {
                stars: Some(first),
                name: second,
                generics,
            })
        } else {
            Err(Error::Parse(
                second.span,
                "type names must be normal".to_string(),
            ))
        }
    } else {
        Err(Error::Parse(
            first.span,
            "type names must be normal".to_string(),
        ))
    }
}

fn parse_type_generics(tokens: &mut Tokens) -> Result<Option<TypeGenerics>, Error> {
    if tokens.peek().is_lbrack() {
        let lbrack_span = tokens.next()?.span();

        let mut types = Vec::new();
        while !tokens.peek().is_rbrack() {
            types.push(parse_type(tokens)?);
        }

        let rbrack_span = tokens.next()?.span();

        Ok(Some(TypeGenerics {
            types,
            lbrack_span,
            rbrack_span,
        }))
    } else {
        Ok(None)
    }
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

fn parse_test(tokens: &mut Tokens) -> Result<(Vec<Op>, Span), Error> {
    let group = parse_group(tokens)?;
    if !tokens.peek().is_lbrace() {
        return Err(Error::Parse(
            tokens.peek().span(),
            "expected '{'".to_string(),
        ));
    }
    Ok((group, tokens.next()?.span()))
}

fn parse_if(tokens: &mut Tokens) -> Result<Stmt, Error> {
    let if_span = tokens.next()?.span();

    let (test, lbrace_span) = parse_test(tokens)?;
    let (body, rbrace_span) = parse_body(tokens)?;

    if tokens.peek().is_else() {
        let else_span = tokens.next()?.span();
        match tokens.peek() {
            Token::LBrace(_) => {
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
                    is_else_if: false,
                })
            }
            Token::If(_) => {
                let mut else_body = parse_if(tokens)?;
                else_body.set_else_if();
                let (else_lbrace_span, else_rbrace_span) = else_body.brace_spans();
                Ok(Stmt::If {
                    test,
                    body,
                    if_span,
                    lbrace_span,
                    rbrace_span,
                    else_part: Some(ElsePart {
                        body: vec![else_body],
                        else_span,
                        lbrace_span: else_lbrace_span,
                        rbrace_span: else_rbrace_span,
                    }),
                    is_else_if: false,
                })
            }
            other => Err(Error::Parse(
                other.span(),
                "expected '{' or 'if'".to_string(),
            )),
        }
    } else {
        Ok(Stmt::If {
            test,
            body,
            if_span,
            lbrace_span,
            rbrace_span,
            else_part: None,
            is_else_if: false,
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
    let mut names: Vec<Name> = Vec::new();

    while tokens.peek().is_name() {
        let name = parse_name(tokens)?;
        for prev in &names {
            if prev.name != "_" && prev.name == name.name {
                return Err(Error::Parse(
                    name.span,
                    format!("duplicate name '{}' in let statement", name.name),
                ));
            }
        }
        names.push(name);
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

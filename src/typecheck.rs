use crate::ast::Item;
use crate::ast::{self, *};
use crate::builtins::BUILTINS;
use crate::error::{Error, Note};
use crate::lex::Span;

use std::collections::{hash_map::Entry, HashMap};
use std::fmt;

#[derive(Clone)]
pub enum Type {
    Primitive(Primitive),
    Pointer(Box<Type>),
    Generic(usize),
    Custom(Name),
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
            Type::Custom(name) => write!(f, "{:?}", name),
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Primitive(s), Type::Primitive(o)) => s == o,
            (Type::Pointer(s), Type::Pointer(o)) => s == o,
            (Type::Generic(s), Type::Generic(o)) => s == o,
            (Type::Custom(s), Type::Custom(o)) => s.name == o.name,
            _ => false,
        }
    }
}

impl Type {
    fn new(ty: ast::Type, generics: &Option<Generics>) -> Type {
        match ty {
            ast::Type::Normal(name) => match name.name {
                "byte" => Type::Primitive(Primitive::Byte),
                "int" => Type::Primitive(Primitive::Int),
                "float" => Type::Primitive(Primitive::Float),
                "bool" => Type::Primitive(Primitive::Bool),
                _ => {
                    if let Some(generics) = generics {
                        if let Some(i) = generics.names.iter().position(|gen| gen.name == name.name)
                        {
                            Type::Generic(i)
                        } else {
                            Type::Custom(name)
                        }
                    } else {
                        Type::Custom(name)
                    }
                }
            },
            ast::Type::Pointer(name, depth) => {
                let mut ty = Type::new(ast::Type::Normal(name), generics);
                for _ in 0..depth {
                    ty = Type::Pointer(Box::new(ty));
                }
                ty
            }
        }
    }

    fn compatible(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Generic(_), _) | (_, Type::Generic(_)) => true,
            (Type::Primitive(s), Type::Primitive(o)) => s == o,
            (Type::Custom(s), Type::Custom(o)) => s.name == o.name,
            (Type::Pointer(s), Type::Pointer(o)) => s.compatible(o),
            _ => false,
        }
    }

    fn bind(&self, other: &Type) -> Result<Option<(usize, Type)>, ()> {
        match (self, other) {
            (Type::Generic(index), other) => Ok(Some((*index, other.clone()))),
            (Type::Custom(self_name), Type::Custom(other_name)) => {
                if self_name.name == other_name.name {
                    Ok(None)
                } else {
                    Err(())
                }
            }
            (Type::Primitive(self_prim), Type::Primitive(other_prim)) => {
                if self_prim == other_prim {
                    Ok(None)
                } else {
                    Err(())
                }
            }
            (Type::Pointer(self_ty), Type::Pointer(other_ty)) => self_ty.bind(other_ty),
            _ => Err(()),
        }
    }

    fn substitute(&self, bindings: &[Type]) -> Type {
        match self {
            Type::Generic(index) => bindings[*index].clone(),
            Type::Pointer(ty) => Type::Pointer(Box::new(ty.substitute(bindings))),
            other => other.clone(),
        }
    }

    fn generic_index(&self) -> Option<usize> {
        match self {
            Type::Generic(index) => Some(*index),
            Type::Pointer(ty) => ty.generic_index(),
            _ => None,
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Primitive {
    Int,   // i32
    Float, // f32
    Byte,  // u8
    Bool,  // i1
}

#[derive(Clone)]
pub struct Signature {
    pub name: Name,
    pub is_builtin: bool,
    pub params: Vec<Type>,
    pub returns: Vec<Type>,
}

impl fmt::Debug for Signature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} -> {:?}", self.params, self.returns)
    }
}

impl Signature {
    pub fn new(name: Name, is_builtin: bool, params: Vec<Type>, returns: Vec<Type>) -> Signature {
        Signature {
            name,
            is_builtin,
            params,
            returns,
        }
    }
}

// TODO: combine signatures and let_binds into one Vec<HashMap>
struct Context {
    signatures: HashMap<&'static str, Vec<Signature>>,
    let_binds: Vec<HashMap<&'static str, Type>>,
}

impl Context {
    fn new() -> Context {
        Context {
            signatures: BUILTINS.clone(),
            let_binds: Vec::new(),
        }
    }

    fn init_signatures(&mut self, unit: &[Item]) -> Result<(), Error> {
        for item in unit {
            let signature = get_signature(item)?;
            self.insert_signature(signature)?;
        }
        Ok(())
    }

    fn insert_signature(&mut self, signature: Signature) -> Result<(), Error> {
        match self.signatures.entry(signature.name.name) {
            Entry::Occupied(mut o) => {
                check_for_conflicts(o.get(), &signature)?;
                o.get_mut().push(signature);
                Ok(())
            }
            Entry::Vacant(v) => {
                v.insert(vec![signature]);
                Ok(())
            }
        }
    }

    fn check_unit(&mut self, unit: &[Item]) -> Result<(), Error> {
        for item in unit {
            self.check_item(item)?;
        }
        Ok(())
    }

    fn update_stack(&self, stack: &mut Vec<Type>, name: Name) -> Result<(), Error> {
        if let Some(ty) = self.get_let_bind_type(name) {
            stack.push(ty);
            return Ok(());
        }

        let (signature, bindings) = self.get_signature_and_bindings(stack, name)?;
        for _ in 0..signature.params.len() {
            stack.pop();
        }
        for ret in signature.returns {
            stack.push(ret.substitute(&bindings));
        }
        Ok(())
    }

    fn get_let_bind_type(&self, name: Name) -> Option<Type> {
        for let_bind in self.let_binds.iter().rev() {
            if let Some(ty) = let_bind.get(name.name) {
                return Some(ty.clone());
            }
        }
        None
    }

    fn get_signature_and_bindings(
        &self,
        stack: &[Type],
        name: Name,
    ) -> Result<(Signature, Vec<Type>), Error> {
        if let Some(signatures) = self.signatures.get(name.name) {
            for signature in signatures {
                if let Some(bindings) = self.get_bindings(stack, signature) {
                    return Ok((signature.clone(), bindings));
                }
            }
            let mut notes = vec![Note::new(None, format!("stack is {:?}", stack))];
            for signature in signatures {
                notes.push(Note::new(
                    None,
                    format!("'{}' has signature {:?}", name.name, signature),
                ));
            }
            Err(Error::Type(
                name.span,
                format!("no variant of '{}' matches the stack", name.name),
                notes,
            ))
        } else {
            Err(Error::Type(
                name.span,
                format!("symbol '{}' does not exists", name.name),
                vec![],
            ))
        }
    }

    fn get_bindings(&self, stack: &[Type], signature: &Signature) -> Option<Vec<Type>> {
        if signature.params.len() > stack.len() {
            None
        } else {
            let mut bindings = vec![None; signature.params.len()];
            for (arg, param) in stack.iter().rev().zip(signature.params.iter().rev()) {
                match param.bind(arg) {
                    Ok(Some((index, ty))) => {
                        if let Some(bound) = &bindings[index] {
                            if bound != &ty {
                                return None;
                            }
                        } else {
                            bindings[index] = Some(ty);
                        }
                    }
                    Ok(None) => {}
                    Err(_) => return None,
                }
            }
            Some(bindings.into_iter().flatten().collect())
        }
    }

    fn check_item(&mut self, item: &Item) -> Result<(), Error> {
        match item {
            Item::Function {
                body, rbrace_span, ..
            } => {
                let signature = get_signature(item)?;
                let mut stack = init_stack(&signature);
                for stmt in body {
                    self.check_stmt(&mut stack, stmt)?;
                }
                check_stack(&mut stack, &signature, *rbrace_span)?;
                Ok(())
            }
        }
    }

    fn check_stmt(&mut self, stack: &mut Vec<Type>, stmt: &Stmt) -> Result<(), Error> {
        match stmt {
            Stmt::Group(ops, _) => {
                for op in ops {
                    self.check_op(stack, op)?;
                }
                Ok(())
            }
            Stmt::If {
                test,
                body,
                lbrace_span,
                rbrace_span,
                else_part,
                ..
            } => self.check_if(stack, test, body, *lbrace_span, *rbrace_span, else_part),
            Stmt::Let {
                names,
                body,
                let_span,
                ..
            } => self.check_let(stack, names, body, *let_span),
            Stmt::For {
                low,
                high,
                body,
                to_span,
                lbrace_span,
                rbrace_span,
                ..
            } => self.check_for(stack, low, high, body, *to_span, *lbrace_span, *rbrace_span),
            Stmt::While {
                test,
                body,
                lbrace_span,
                rbrace_span,
                ..
            } => self.check_while(stack, test, body, *lbrace_span, *rbrace_span),
        }
    }

    fn check_if(
        &mut self,
        stack: &mut Vec<Type>,
        test: &[Op],
        body: &[Stmt],
        lbrace_span: Span,
        rbrace_span: Span,
        else_part: &Option<ElsePart>,
    ) -> Result<(), Error> {
        for op in test {
            self.check_op(stack, op)?;
        }

        if stack.last() != Some(&Type::Primitive(Primitive::Bool)) {
            return Err(Error::Type(
                lbrace_span,
                "expected a 'bool' for if statement".to_string(),
                vec![Note::new(None, format!("stack is {:?}", stack))],
            ));
        }
        stack.pop();

        if let Some(else_part) = else_part {
            let stack_before = format!("before if block, stack is {:?}", stack);
            let mut else_stack = stack.clone();
            for stmt in body {
                self.check_stmt(stack, stmt)?;
            }
            for stmt in &else_part.body {
                self.check_stmt(&mut else_stack, stmt)?;
            }

            if stack != &else_stack {
                return Err(Error::Type(
                    else_part.rbrace_span,
                    "if and else blocks have mismatched stacks".to_string(),
                    vec![
                        Note::new(None, stack_before),
                        Note::new(None, format!("after if block, stack is {:?}", stack)),
                        Note::new(None, format!("after else block, stack is {:?}", else_stack)),
                    ],
                ));
            }
        } else {
            let stack_before = stack.clone();
            for stmt in body {
                self.check_stmt(stack, stmt)?;
            }
            if stack != &stack_before {
                return Err(Error::Type(
                    rbrace_span,
                    "stack does not match the stack before the if block".to_string(),
                    vec![
                        Note::new(
                            None,
                            format!("before if block, stack is {:?}", stack_before),
                        ),
                        Note::new(None, format!("after if block, stack is {:?}", stack)),
                    ],
                ));
            }
        }
        Ok(())
    }

    fn check_let(
        &mut self,
        stack: &mut Vec<Type>,
        names: &[Name],
        body: &[Stmt],
        let_span: Span,
    ) -> Result<(), Error> {
        if stack.len() < names.len() {
            return Err(Error::Type(
                let_span,
                format!(
                    "let statement requires {} value{}, but the stack {}has {}",
                    names.len(),
                    if names.len() == 1 { "" } else { "s" },
                    if stack.is_empty() { "" } else { "only " },
                    stack.len()
                ),
                vec![Note::new(None, format!("stack is {stack:?}"))],
            ));
        }

        let mut binds = HashMap::new();
        for name in names.iter().rev() {
            binds.insert(name.name, stack.pop().unwrap());
        }
        self.let_binds.push(binds);

        for stmt in body {
            self.check_stmt(stack, stmt)?;
        }

        self.let_binds.pop();

        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    fn check_for(
        &mut self,
        stack: &mut Vec<Type>,
        low: &[Op],
        high: &[Op],
        body: &[Stmt],
        to_span: Span,
        lbrace_span: Span,
        rbrace_span: Span,
    ) -> Result<(), Error> {
        let mut stack_before = stack.clone();
        for op in low {
            self.check_op(stack, op)?;
        }

        let iter_type = match stack.pop() {
            Some(Type::Primitive(Primitive::Int)) => Type::Primitive(Primitive::Int),
            Some(Type::Primitive(Primitive::Byte)) => Type::Primitive(Primitive::Byte),
            Some(ty) => {
                stack_before.push(ty);
                return Err(Error::Type(
                    to_span,
                    "lower bound in for statement should be an 'int' or 'byte'".to_string(),
                    vec![Note::new(None, format!("stack is {stack_before:?}"))],
                ));
            }
            None => {
                return Err(Error::Type(
                    to_span,
                    "lower bound in for statement should be an 'int' or 'byte'".to_string(),
                    vec![Note::new(None, "stack is empty".to_string())],
                ))
            }
        };

        for op in high {
            self.check_op(stack, op)?;
        }

        match stack.pop() {
            Some(ty) if ty == iter_type => {}
            Some(ty) => {
                stack_before.push(ty);
                return Err(Error::Type(
                    lbrace_span,
                    format!("upper bound in for statement should have type '{iter_type:?}'"),
                    vec![Note::new(None, format!("stack is {stack_before:?}"))],
                ));
            }
            None => {
                return Err(Error::Type(
                    lbrace_span,
                    format!("upper bound in for statement should have type '{iter_type:?}'"),
                    vec![Note::new(None, "stack is empty".to_string())],
                ))
            }
        };

        stack.push(iter_type);
        for stmt in body {
            self.check_stmt(stack, stmt)?;
        }

        if stack != &stack_before {
            Err(Error::Type(
                rbrace_span,
                "stack does not match the stack before the for block".to_string(),
                vec![
                    Note::new(None, format!("before for block, stack is {stack_before:?}")),
                    Note::new(None, format!("after for block, stack is {stack:?}")),
                ],
            ))
        } else {
            Ok(())
        }
    }

    fn check_while(
        &mut self,
        stack: &mut Vec<Type>,
        test: &[Op],
        body: &[Stmt],
        lbrace_span: Span,
        rbrace_span: Span,
    ) -> Result<(), Error> {
        let stack_before = stack.clone();

        for op in test {
            self.check_op(stack, op)?;
        }

        if stack.last() != Some(&Type::Primitive(Primitive::Bool)) {
            return Err(Error::Type(
                lbrace_span,
                "expected a 'bool' for while statement".to_string(),
                vec![Note::new(None, format!("stack is {:?}", stack))],
            ));
        }
        stack.pop();

        for stmt in body {
            self.check_stmt(stack, stmt)?;
        }

        if stack != &stack_before {
            Err(Error::Type(
                rbrace_span,
                "stack does not match the stack before the while block".to_string(),
                vec![
                    Note::new(
                        None,
                        format!("before while block, stack is {stack_before:?}"),
                    ),
                    Note::new(None, format!("after while block, stack is {stack:?}")),
                ],
            ))
        } else {
            Ok(())
        }
    }

    fn check_op(&mut self, stack: &mut Vec<Type>, op: &Op) -> Result<(), Error> {
        match op {
            Op::Int(_, _) => {
                stack.push(Type::Primitive(Primitive::Int));
                Ok(())
            }
            Op::Float(_, _) => {
                stack.push(Type::Primitive(Primitive::Float));
                Ok(())
            }
            Op::Bool(_, _) => {
                stack.push(Type::Primitive(Primitive::Bool));
                Ok(())
            }
            Op::Char(_, _) => {
                stack.push(Type::Primitive(Primitive::Int));
                Ok(())
            }
            Op::String(_, _) => {
                stack.push(Type::Pointer(Box::new(Type::Primitive(Primitive::Byte))));
                Ok(())
            }
            Op::Name(name) => self.update_stack(stack, *name),
            Op::Expr(expr, _) => {
                stack.push(self.type_of(expr)?);
                Ok(())
            }
        }
    }

    // TODO: use self.signatures instead of BUILTINS so that you can overload
    // operators even in expressions
    fn type_of(&mut self, expr: &Expr) -> Result<Type, Error> {
        macro_rules! binop {
            ($op:expr, $left:expr, $right:expr, $span:expr) => {{
                let (left, right) = (self.type_of($left)?, self.type_of($right)?);
                let params = vec![left, right];
                for signature in BUILTINS.get($op).unwrap() {
                    if signature.params == params {
                        return Ok(signature.returns[0].clone());
                    }
                }

                let [left, right]: [Type; 2] = params.try_into().unwrap();
                Err(Error::Type(
                    *$span,
                    format!(
                        "operator '{}' is not defined for '{left:?}' and '{right:?}'",
                        $op
                    ),
                    vec![],
                ))
            }};
        }

        match expr {
            Expr::Int(_, _) => Ok(Type::Primitive(Primitive::Int)),
            Expr::Float(_, _) => Ok(Type::Primitive(Primitive::Float)),
            Expr::Bool(_, _) => Ok(Type::Primitive(Primitive::Bool)),
            Expr::Char(_, _) => Ok(Type::Primitive(Primitive::Int)),
            Expr::Name(name) => {
                if let Some(ty) = self.get_let_bind_type(*name) {
                    Ok(ty)
                } else {
                    Err(Error::Type(
                        name.span,
                        format!("symbol '{}' does not exists", name.name),
                        vec![],
                    ))
                }
            }
            Expr::Add(left, right, span) => binop!("+", left, right, span),
            Expr::And(left, right, span) => binop!("&", left, right, span),
            Expr::Divide(left, right, span) => binop!("/", left, right, span),
            Expr::Equal(left, right, span) => binop!("==", left, right, span),
            Expr::GreaterEqual(left, right, span) => binop!(">=", left, right, span),
            Expr::GreaterThan(left, right, span) => binop!(">", left, right, span),
            Expr::LessEqual(left, right, span) => binop!("<=", left, right, span),
            Expr::LessThan(left, right, span) => binop!("<", left, right, span),
            Expr::Modulo(left, right, span) => binop!("%", left, right, span),
            Expr::Multiply(left, right, span) => binop!("*", left, right, span),
            Expr::NotEqual(left, right, span) => binop!("!=", left, right, span),
            Expr::Or(left, right, span) => binop!("|", left, right, span),
            Expr::Subtract(left, right, span) => binop!("-", left, right, span),
            Expr::Xor(left, right, span) => binop!("^", left, right, span),
            Expr::Not(expr, span) => {
                let ty = self.type_of(expr)?;
                match ty {
                    Type::Primitive(Primitive::Bool) => Ok(Type::Primitive(Primitive::Bool)),
                    Type::Primitive(Primitive::Byte) => Ok(Type::Primitive(Primitive::Byte)),
                    Type::Primitive(Primitive::Int) => Ok(Type::Primitive(Primitive::Int)),
                    _ => Err(Error::Type(
                        *span,
                        format!("operator '!' is not defined for '{ty:?}'"),
                        vec![],
                    )),
                }
            }
            Expr::Negate(expr, span) => {
                let ty = self.type_of(expr)?;
                match ty {
                    Type::Primitive(Primitive::Byte) => Ok(Type::Primitive(Primitive::Byte)),
                    Type::Primitive(Primitive::Int) => Ok(Type::Primitive(Primitive::Int)),
                    Type::Primitive(Primitive::Float) => Ok(Type::Primitive(Primitive::Float)),
                    _ => Err(Error::Type(
                        *span,
                        format!("operator '-' is not defined for '{ty:?}'"),
                        vec![],
                    )),
                }
            }
            Expr::Group(group, span) => {
                let mut stack = Vec::new();
                for op in group {
                    self.check_op(&mut stack, op)?;
                }
                match stack.len() {
                    1 => Ok(stack.pop().unwrap()),
                    0 => Err(Error::Type(
                        *span,
                        "expression group yields no values".to_string(),
                        vec![],
                    )),
                    _ => Err(Error::Type(
                        *span,
                        "expression group yields multiple values".to_string(),
                        vec![Note::new(None, format!("stack is {stack:?}"))],
                    )),
                }
            }
        }
    }

    /*
    fn define_structs(&mut self, unit: &Vec<Item>) -> Result<(), Error> {
        todo!()
    }
    */
}

fn init_stack(signature: &Signature) -> Vec<Type> {
    signature.params.clone()
}

fn check_stack(stack: &mut Vec<Type>, signature: &Signature, span: Span) -> Result<(), Error> {
    if stack == &signature.returns {
        Ok(())
    } else {
        Err(Error::Type(
            span,
            "stack does not match the declared return type".to_string(),
            vec![
                Note::new(None, format!("expected {:?}", signature.returns)),
                Note::new(None, format!("found {:?}", stack)),
            ],
        ))
    }
}

fn check_for_conflicts(
    existing_signatures: &[Signature],
    new_signature: &Signature,
) -> Result<(), Error> {
    for existing in existing_signatures {
        if conflict(&existing.params, &new_signature.params) {
            return Err(Error::Type(
                new_signature.name.span,
                "signature conflicts with a previous definition".to_string(),
                vec![if existing.is_builtin {
                    Note::new(
                        None,
                        format!("'{}' is a builtin function", existing.name.name),
                    )
                } else {
                    Note::new(
                        Some(existing.name.span),
                        "previous definition is here".to_string(),
                    )
                }],
            ));
        }
    }
    Ok(())
}

fn get_signature(item: &Item) -> Result<Signature, Error> {
    match item {
        Item::Function {
            name,
            generics,
            params,
            returns,
            ..
        } => {
            let params: Vec<_> = params
                .iter()
                .copied()
                .map(|t| Type::new(t, generics))
                .collect();
            let returns = returns
                .iter()
                .copied()
                .map(|t| Type::new(t, generics))
                .collect();

            if let Some(generics) = generics {
                let mut seen = vec![false; generics.names.len()];
                for param in &params {
                    if let Some(index) = param.generic_index() {
                        seen[index] = true;
                    }
                }
                for (i, seen) in seen.into_iter().enumerate() {
                    if !seen {
                        return Err(Error::Parse(
                            generics.names[i].span,
                            format!(
                                "generic argument '{}' does not appear in function parameters",
                                generics.names[i].name
                            ),
                        ));
                    }
                }
            }

            Ok(Signature {
                name: *name,
                is_builtin: false,
                params,
                returns,
            })
        }
    }
}

fn conflict(params_a: &[Type], params_b: &[Type]) -> bool {
    params_a
        .iter()
        .rev()
        .zip(params_b.iter().rev())
        .all(|(a, b)| a.compatible(b))
}

pub fn typecheck(unit: &[Item]) -> Result<(), Error> {
    let mut context = Context::new();

    context.init_signatures(unit)?;
    context.check_unit(unit)?;

    Ok(())
}

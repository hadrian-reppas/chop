use crate::ast::Item;
use crate::ast::*;
use crate::builtins::BUILTINS;
use crate::codegen::Type;
use crate::error::{Error, Note};
use crate::lex::Span;

use std::collections::{hash_map::Entry, HashMap, HashSet};
use std::fmt;

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum GType {
    Primitive(Primitive),
    Pointer(Box<GType>),
    Generic(usize),
    Custom(Name),
}

impl fmt::Debug for GType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GType::Primitive(Primitive::Int) => write!(f, "int"),
            GType::Primitive(Primitive::Float) => write!(f, "float"),
            GType::Primitive(Primitive::Byte) => write!(f, "byte"),
            GType::Primitive(Primitive::Bool) => write!(f, "bool"),
            GType::Pointer(ty) => write!(f, "*{ty:?}"),
            GType::Generic(n) => write!(f, "T{}", n),
            GType::Custom(name) => write!(f, "{:?}", name),
        }
    }
}

impl GType {
    fn new(ty: PType, generics: &Option<Generics>) -> GType {
        match ty {
            PType::Normal(name) => match name.name {
                "byte" => GType::Primitive(Primitive::Byte),
                "int" => GType::Primitive(Primitive::Int),
                "float" => GType::Primitive(Primitive::Float),
                "bool" => GType::Primitive(Primitive::Bool),
                _ => {
                    if let Some(generics) = generics {
                        if let Some(i) = generics.names.iter().position(|gen| gen.name == name.name)
                        {
                            GType::Generic(i)
                        } else {
                            GType::Custom(name)
                        }
                    } else {
                        GType::Custom(name)
                    }
                }
            },
            PType::Pointer(name, depth) => {
                let mut ty = GType::new(PType::Normal(name), generics);
                for _ in 0..depth {
                    ty = GType::Pointer(Box::new(ty));
                }
                ty
            }
        }
    }

    fn compatible(&self, other: &GType) -> bool {
        match (self, other) {
            (GType::Generic(_), _) | (_, GType::Generic(_)) => true,
            (GType::Primitive(s), GType::Primitive(o)) => s == o,
            (GType::Custom(s), GType::Custom(o)) => s.name == o.name,
            (GType::Pointer(s), GType::Pointer(o)) => s.compatible(o),
            _ => false,
        }
    }

    fn bind(&self, other: &GType) -> Result<Option<(usize, GType)>, ()> {
        match (self, other) {
            (GType::Generic(index), other) => Ok(Some((*index, other.clone()))),
            (GType::Custom(self_name), GType::Custom(other_name)) => {
                if self_name.name == other_name.name {
                    Ok(None)
                } else {
                    Err(())
                }
            }
            (GType::Primitive(self_prim), GType::Primitive(other_prim)) => {
                if self_prim == other_prim {
                    Ok(None)
                } else {
                    Err(())
                }
            }
            (GType::Pointer(self_ty), GType::Pointer(other_ty)) => self_ty.bind(other_ty),
            _ => Err(()),
        }
    }

    pub fn bind_(&self, other: Type) -> Result<Option<(usize, Type)>, ()> {
        match (self, other) {
            (GType::Generic(index), other) => Ok(Some((*index, other))),
            (GType::Custom(self_name), Type::Custom(other_name, depth)) => {
                if self_name.name == other_name && depth == 0 {
                    Ok(None)
                } else {
                    Err(())
                }
            }
            (GType::Primitive(Primitive::Int), Type::Int(0)) => Ok(None),
            (GType::Primitive(Primitive::Bool), Type::Bool(0)) => Ok(None),
            (GType::Primitive(Primitive::Float), Type::Float(0)) => Ok(None),
            (GType::Primitive(Primitive::Byte), Type::Byte(0)) => Ok(None),
            (GType::Pointer(_), Type::Bool(0)) => Err(()),
            (GType::Pointer(_), Type::Byte(0)) => Err(()),
            (GType::Pointer(_), Type::Int(0)) => Err(()),
            (GType::Pointer(_), Type::Float(0)) => Err(()),
            (GType::Pointer(_), Type::Custom(_, 0)) => Err(()),
            (GType::Pointer(self_ty), Type::Bool(depth)) => self_ty.bind_(Type::Bool(depth - 1)),
            (GType::Pointer(self_ty), Type::Byte(depth)) => self_ty.bind_(Type::Byte(depth - 1)),
            (GType::Pointer(self_ty), Type::Int(depth)) => self_ty.bind_(Type::Int(depth - 1)),
            (GType::Pointer(self_ty), Type::Float(depth)) => self_ty.bind_(Type::Float(depth - 1)),
            (GType::Pointer(self_ty), Type::Custom(name, depth)) => {
                self_ty.bind_(Type::Custom(name, depth - 1))
            }
            _ => Err(()),
        }
    }

    fn substitute(&self, bindings: &[GType]) -> GType {
        match self {
            GType::Generic(index) => bindings[*index].clone(),
            GType::Pointer(ty) => GType::Pointer(Box::new(ty.substitute(bindings))),
            other => other.clone(),
        }
    }

    pub fn substitute_(&self, bindings: &[Type]) -> Type {
        match self {
            GType::Generic(index) => bindings[*index],
            GType::Pointer(ty) => ty.substitute_(bindings).inc(),
            GType::Custom(name) => Type::Custom(name.name, 0),
            GType::Primitive(Primitive::Bool) => Type::Bool(0),
            GType::Primitive(Primitive::Byte) => Type::Byte(0),
            GType::Primitive(Primitive::Int) => Type::Int(0),
            GType::Primitive(Primitive::Float) => Type::Float(0),
        }
    }

    fn generic_index(&self) -> Option<usize> {
        match self {
            GType::Generic(index) => Some(*index),
            GType::Pointer(ty) => ty.generic_index(),
            _ => None,
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub enum Primitive {
    Int,   // i32
    Float, // f32
    Byte,  // u8
    Bool,  // i1
}

#[derive(Clone)]
pub struct GSignature {
    pub name: Name,
    pub kind: Kind,
    pub params: Vec<GType>,
    pub returns: Vec<GType>,
}

impl fmt::Debug for GSignature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} -> {:?}", self.params, self.returns)
    }
}

impl GSignature {
    pub fn new(name: Name, kind: Kind, params: Vec<GType>, returns: Vec<GType>) -> GSignature {
        GSignature {
            name,
            kind,
            params,
            returns,
        }
    }
}

#[derive(Clone)]
pub enum Kind {
    Builtin,
    Auto,
    Custom(Vec<Stmt>),
}

//                 name          id
pub type FnInfo = (&'static str, usize);
//                   name          id     bindings
pub type CallInfo = (&'static str, usize, Vec<GType>);

struct CallGraph {
    active: FnInfo,
    graph: HashMap<FnInfo, HashSet<CallInfo>>,
}

impl CallGraph {
    fn new() -> CallGraph {
        CallGraph {
            active: ("", 0),
            graph: HashMap::new(),
        }
    }

    fn set_active(&mut self, name: &'static str, id: usize) {
        self.active = (name, id);
    }

    fn insert(&mut self, name: &'static str, id: usize, bindings: Vec<GType>) {
        if let Some(set) = self.graph.get_mut(&self.active) {
            set.insert((name, id, bindings));
        } else {
            let set = HashSet::from([(name, id, bindings)]);
            self.graph.insert(self.active, set);
        }
    }
}

// TODO: combine signatures and let_binds into one Vec<HashMap>
struct Context {
    signatures: HashMap<&'static str, Vec<GSignature>>,
    let_binds: Vec<HashMap<&'static str, GType>>,
    call_graph: CallGraph,
}

impl Context {
    fn new() -> Context {
        Context {
            signatures: BUILTINS.clone(),
            let_binds: Vec::new(),
            call_graph: CallGraph::new(),
        }
    }

    fn init_signatures(&mut self, unit: &[Item]) -> Result<(), Error> {
        for item in unit {
            let signature = get_signature(item)?;
            self.insert_signature(signature)?;
        }
        if let Some(mains) = self.signatures.get("main") {
            if mains.len() == 1 {
                let GSignature {
                    params,
                    returns,
                    name,
                    ..
                } = mains[0].clone();

                if !params.is_empty()
                    && params
                        != [
                            GType::Primitive(Primitive::Int),
                            GType::Pointer(Box::new(GType::Pointer(Box::new(GType::Primitive(
                                Primitive::Byte,
                            ))))),
                        ]
                {
                    Err(Error::Type(
                        name.span,
                        "'main' must have parameters [] or [int, **byte]".to_string(),
                        vec![Note::new(
                            None,
                            format!("'main' has signature {params:?} -> {returns:?}"),
                        )],
                    ))
                } else if !returns.is_empty() && returns != [GType::Primitive(Primitive::Int)] {
                    Err(Error::Type(
                        name.span,
                        "'main' must return [] or [int]".to_string(),
                        vec![Note::new(
                            None,
                            format!("'main' has signature {params:?} -> {returns:?}"),
                        )],
                    ))
                } else {
                    Ok(())
                }
            } else {
                Err(Error::Main(
                    "multiple functions named 'main'".to_string(),
                    mains
                        .iter()
                        .map(|s| Note::new(Some(s.name.span), "'main' defined here".to_string()))
                        .collect(),
                ))
            }
        } else {
            Err(Error::Main("no function named 'main'".to_string(), vec![]))
        }
    }

    fn insert_signature(&mut self, signature: GSignature) -> Result<(), Error> {
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

    fn update_stack(&mut self, stack: &mut Vec<GType>, name: Name) -> Result<(), Error> {
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

    fn get_let_bind_type(&self, name: Name) -> Option<GType> {
        for let_bind in self.let_binds.iter().rev() {
            if let Some(ty) = let_bind.get(name.name) {
                return Some(ty.clone());
            }
        }
        None
    }

    fn get_signature_and_bindings(
        &mut self,
        stack: &[GType],
        name: Name,
    ) -> Result<(GSignature, Vec<GType>), Error> {
        if let Some(signatures) = self.signatures.get(name.name) {
            for (id, signature) in signatures.iter().enumerate() {
                if let Some(bindings) = self.get_bindings(stack, signature) {
                    self.call_graph.insert(name.name, id, bindings.clone());
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

    fn get_bindings(&self, stack: &[GType], signature: &GSignature) -> Option<Vec<GType>> {
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
                name,
                body,
                rbrace_span,
                ..
            } => {
                let signature = get_signature(item)?;
                let id = self
                    .signatures
                    .get(name.name)
                    .unwrap()
                    .iter()
                    .position(|s| s.params == signature.params)
                    .unwrap();
                self.call_graph.set_active(name.name, id);
                let mut stack = init_stack(&signature);
                for stmt in body {
                    self.check_stmt(&mut stack, stmt)?;
                }
                check_stack(&mut stack, &signature, *rbrace_span)?;
                Ok(())
            }
        }
    }

    fn check_stmt(&mut self, stack: &mut Vec<GType>, stmt: &Stmt) -> Result<(), Error> {
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

    // TODO: optional test
    //       if let Some(test) = test {
    //           for op in test {
    //               self.check_op(stack, op)?;
    //           }
    //       }
    //       continue on as before
    fn check_if(
        &mut self,
        stack: &mut Vec<GType>,
        test: &[Op],
        body: &[Stmt],
        lbrace_span: Span,
        rbrace_span: Span,
        else_part: &Option<ElsePart>,
    ) -> Result<(), Error> {
        for op in test {
            self.check_op(stack, op)?;
        }

        if stack.last() != Some(&GType::Primitive(Primitive::Bool)) {
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
        stack: &mut Vec<GType>,
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
            let ty = stack.pop().unwrap();
            if name.name != "_" {
                binds.insert(name.name, ty);
            }
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
        stack: &mut Vec<GType>,
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
            Some(GType::Primitive(Primitive::Int)) => GType::Primitive(Primitive::Int),
            Some(GType::Primitive(Primitive::Byte)) => GType::Primitive(Primitive::Byte),
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

    // TODO: optional test
    //       let stack_before = stack.clone()
    //       assert stack.pop() is bool
    //       check body
    //       assert stack == stack_before
    //       stack.pop()
    //       continue on...
    fn check_while(
        &mut self,
        stack: &mut Vec<GType>,
        test: &[Op],
        body: &[Stmt],
        lbrace_span: Span,
        rbrace_span: Span,
    ) -> Result<(), Error> {
        let stack_before = stack.clone();

        for op in test {
            self.check_op(stack, op)?;
        }

        if stack.last() != Some(&GType::Primitive(Primitive::Bool)) {
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

    fn check_op(&mut self, stack: &mut Vec<GType>, op: &Op) -> Result<(), Error> {
        match op {
            Op::Int(_, _) => {
                stack.push(GType::Primitive(Primitive::Int));
                Ok(())
            }
            Op::Float(_, _) => {
                stack.push(GType::Primitive(Primitive::Float));
                Ok(())
            }
            Op::Bool(_, _) => {
                stack.push(GType::Primitive(Primitive::Bool));
                Ok(())
            }
            Op::Char(_, _) => {
                stack.push(GType::Primitive(Primitive::Int));
                Ok(())
            }
            Op::String(_, _) => {
                stack.push(GType::Pointer(Box::new(GType::Primitive(Primitive::Byte))));
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
    fn type_of(&mut self, expr: &Expr) -> Result<GType, Error> {
        macro_rules! binop {
            ($op:expr, $left:expr, $right:expr, $span:expr) => {{
                let (left, right) = (self.type_of($left)?, self.type_of($right)?);
                let params = vec![left, right];
                for signature in BUILTINS.get($op).unwrap() {
                    if signature.params == params {
                        return Ok(signature.returns[0].clone());
                    }
                }

                let [left, right]: [GType; 2] = params.try_into().unwrap();
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
            Expr::Int(_, _) => Ok(GType::Primitive(Primitive::Int)),
            Expr::Float(_, _) => Ok(GType::Primitive(Primitive::Float)),
            Expr::Bool(_, _) => Ok(GType::Primitive(Primitive::Bool)),
            Expr::Char(_, _) => Ok(GType::Primitive(Primitive::Int)),
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
                    GType::Primitive(Primitive::Bool) => Ok(GType::Primitive(Primitive::Bool)),
                    GType::Primitive(Primitive::Byte) => Ok(GType::Primitive(Primitive::Byte)),
                    GType::Primitive(Primitive::Int) => Ok(GType::Primitive(Primitive::Int)),
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
                    GType::Primitive(Primitive::Byte) => Ok(GType::Primitive(Primitive::Byte)),
                    GType::Primitive(Primitive::Int) => Ok(GType::Primitive(Primitive::Int)),
                    GType::Primitive(Primitive::Float) => Ok(GType::Primitive(Primitive::Float)),
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

fn init_stack(signature: &GSignature) -> Vec<GType> {
    signature.params.clone()
}

fn check_stack(stack: &mut Vec<GType>, signature: &GSignature, span: Span) -> Result<(), Error> {
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
    existing_signatures: &[GSignature],
    new_signature: &GSignature,
) -> Result<(), Error> {
    for existing in existing_signatures {
        if conflict(&existing.params, &new_signature.params) {
            let note = match existing.kind {
                Kind::Builtin => Note::new(
                    None,
                    format!("'{}' is a builtin function", existing.name.name),
                ),
                Kind::Auto => todo!(),
                Kind::Custom(_) => Note::new(
                    Some(existing.name.span),
                    "previous definition is here".to_string(),
                ),
            };
            return Err(Error::Type(
                new_signature.name.span,
                "signature conflicts with a previous definition".to_string(),
                vec![note],
            ));
        }
    }
    Ok(())
}

// fn get_signatures(item: &Item) -> Result<Vec<Signature>, Error>
fn get_signature(item: &Item) -> Result<GSignature, Error> {
    match item {
        Item::Function {
            name,
            generics,
            params,
            returns,
            body,
            ..
        } => {
            let params: Vec<_> = params
                .iter()
                .copied()
                .map(|t| GType::new(t, generics))
                .collect();
            let returns = returns
                .iter()
                .copied()
                .map(|t| GType::new(t, generics))
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

            Ok(GSignature {
                name: *name,
                kind: Kind::Custom(body.clone()),
                params,
                returns,
            })
        }
    }
}

fn conflict(params_a: &[GType], params_b: &[GType]) -> bool {
    params_a
        .iter()
        .rev()
        .zip(params_b.iter().rev())
        .all(|(a, b)| a.compatible(b))
}

pub struct TypeInfo {
    pub signatures: HashMap<&'static str, Vec<GSignature>>,
    pub graph: HashMap<FnInfo, HashSet<CallInfo>>,
}

pub fn typecheck(unit: &[Item]) -> Result<TypeInfo, Error> {
    let mut context = Context::new();

    context.init_signatures(unit)?;
    context.check_unit(unit)?;

    Ok(TypeInfo {
        signatures: context.signatures,
        graph: context.call_graph.graph,
    })
}
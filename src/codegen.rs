use crate::ast::{ElsePart, Expr, Item, Op, Stmt};
use crate::error::Error;
use crate::typecheck::{self, CallInfo};
use crate::typecheck::{Kind, TypeInfo};

use std::collections::{btree_map, hash_map, BTreeMap, HashMap, HashSet};
use std::fmt;
use std::io::{self, Write};

// TODO: this might not have to be a BTreeMap
//                       name                  id     concrete signatures
type Concrete = HashMap<&'static str, BTreeMap<usize, Vec<Signature>>>;
type Done = HashSet<(&'static str, usize, Vec<Type>)>;
type CallGraph = HashMap<(&'static str, usize), HashSet<CallInfo>>;

pub fn generate(out: &mut dyn Write, unit: &[Item], info: &TypeInfo) -> Result<(), Error> {
    generate_unit(out, unit, info).map_err(|err| Error::Io(err.to_string()))
}

fn generate_unit(out: &mut dyn Write, unit: &[Item], info: &TypeInfo) -> io::Result<()> {
    let mut concrete: Concrete = HashMap::new();
    let mut done: Done = HashSet::new();
    fill("main", 0, Vec::new(), info, &mut concrete, &mut done);

    for (name, signatures) in &info.signatures {
        for (id, typecheck::Signature { kind, .. }) in signatures.iter().enumerate() {
            if let Some(signatures) = concrete.get(name) {
                if let Some(signatures) = signatures.get(&id) {
                    for signature in signatures {
                        generate_function(out, name, signature, kind, info)?;
                    }
                }
            }
        }
    }

    Ok(())
}

fn generate_function(
    out: &mut dyn Write,
    name: &str,
    signature: &Signature,
    kind: &Kind,
    info: &TypeInfo,
) -> io::Result<()> {
    match kind {
        Kind::Builtin => generate_builtin(out, name, signature),
        Kind::Auto => generate_auto(out, name, signature, info),
        Kind::Custom(body) => generate_custom(out, name, signature, body, info),
    }
}

struct Context {
    stack: Vec<(usize, Type)>,
    counter: usize,
}

impl Context {
    fn new(signature: &Signature) -> Context {
        Context {
            stack: signature.params.iter().cloned().enumerate().collect(),
            counter: signature.params.len(),
        }
    }

    fn clean_up(&self, out: &mut dyn Write) -> io::Result<()> {
        for (i, (reg, ty)) in self.stack.iter().enumerate() {
            write!(out, "  store ")?;
            generate_type(out, *ty)?;
            write!(out, " %x{reg}, ")?;
            generate_type(out, *ty)?;
            writeln!(out, "* %o{i}")?;
        }
        Ok(())
    }

    fn push_ref(&mut self, out: &mut dyn Write) -> io::Result<()> {
        let (reg, ty) = *self.stack.last().unwrap();
        write!(out, "  %x{} = alloca ", self.counter)?;
        generate_type(out, ty)?;
        write!(out, "\n  store ")?;
        generate_type(out, ty)?;
        write!(out, " %x{}, ", reg)?;
        generate_type(out, ty)?;
        self.stack.push((self.counter, ty.inc()));
        self.counter += 1;
        writeln!(out, "* %x{}", self.counter - 1)
    }

    fn push(&mut self, ty: Type) -> usize {
        self.stack.push((self.counter, ty));
        self.counter += 1;
        self.counter - 1
    }

    fn pop(&mut self) -> (usize, Type) {
        self.stack.pop().unwrap()
    }

    fn reg(&mut self) -> usize {
        self.counter += 1;
        self.counter - 1
    }

    fn alloc(&mut self, out: &mut dyn Write, ty: Type) -> io::Result<usize> {
        write!(out, "  %x{} = alloca ", self.counter)?;
        generate_type(out, ty)?;
        writeln!(out)?;
        self.counter += 1;
        Ok(self.counter - 1)
    }
}

fn generate_custom(
    out: &mut dyn Write,
    name: &str,
    signature: &Signature,
    body: &[Stmt],
    info: &TypeInfo,
) -> io::Result<()> {
    if name == "main" && !signature.returns.is_empty() {
        write!(out, "define i64 @")?;
    } else {
        write!(out, "define void @")?;
    }
    mangled_name(out, name, &signature.params)?;
    params(out, signature)?;
    writeln!(out, " {{")?;

    let mut context = Context::new(signature);

    for stmt in body {
        generate_stmt(out, stmt, &mut context, info)?;
    }

    context.clean_up(out)?;

    if name == "main" && !signature.returns.is_empty() {
        let (reg, ty) = context.pop();
        write!(out, "  ret ")?;
        generate_type(out, ty)?;
        writeln!(out, " %x{}\n}}", reg)
    } else {
        writeln!(out, "  ret void\n}}")
    }
}

fn generate_stmt(
    out: &mut dyn Write,
    stmt: &Stmt,
    context: &mut Context,
    info: &TypeInfo,
) -> io::Result<()> {
    match stmt {
        Stmt::Group(ops, _) => {
            for op in ops {
                generate_op(out, op, context, info)?;
            }
            Ok(())
        }
        Stmt::If {
            test,
            body,
            else_part,
            ..
        } => generate_if(out, test, body, else_part, context, info),
        Stmt::While {
            test,
            body,
            while_span,
            lbrace_span,
            rbrace_span,
        } => todo!(),
        Stmt::For {
            low,
            high,
            body,
            for_span,
            to_span,
            lbrace_span,
            rbrace_span,
        } => todo!(),
        Stmt::Let {
            names,
            body,
            let_span,
            lbrace_span,
            rbrace_span,
        } => todo!(),
    }
}

fn generate_if(
    out: &mut dyn Write,
    test: &[Op],
    body: &[Stmt],
    else_part: &Option<ElsePart>,
    context: &mut Context,
    info: &TypeInfo,
) -> io::Result<()> {
    todo!()
}

fn generate_op(
    out: &mut dyn Write,
    op: &Op,
    context: &mut Context,
    info: &TypeInfo,
) -> io::Result<()> {
    match op {
        Op::Int(i, _) => {
            let r = context.reg();
            let reg = context.push(Type::Int(0));
            writeln!(out, "  %x{r} = alloca i64")?;
            writeln!(out, "  store i64 {i}, i64* %x{r}")?;
            writeln!(out, "  %x{reg} = load i64, i64* %x{r}")
        }
        Op::Float(f, _) => todo!(),
        Op::Bool(b, _) => todo!(),
        Op::Char(c, _) => todo!(),
        Op::String(_, _) => todo!(),
        Op::Name(name) => generate_call(out, name.name, context, info),
        Op::Expr(expr, _) => generate_expr(out, expr, context, info),
    }
}

fn generate_call(
    out: &mut dyn Write,
    name: &str,
    context: &mut Context,
    info: &TypeInfo,
) -> io::Result<()> {
    for signature in info.signatures.get(name).unwrap() {
        if let Some((params, returns)) = get_sig(context, signature) {
            let ret_regs: Vec<_> = returns
                .iter()
                .map(|ty| context.alloc(out, *ty))
                .collect::<Result<_, _>>()?;
            let mut args = Vec::new();
            for _ in 0..params.len() {
                args.push(context.pop());
            }
            args.reverse();
            write!(out, "  call void @")?;
            mangled_name(out, name, &params)?;
            write!(out, "(")?;
            for (i, (reg, ty)) in args.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ")?;
                }
                generate_type(out, *ty)?;
                write!(out, " %x{}", reg)?;
            }
            for (i, (reg, ty)) in ret_regs.iter().zip(returns.iter()).enumerate() {
                if i > 0 || !args.is_empty() {
                    write!(out, ", ")?;
                }
                generate_type(out, *ty)?;
                write!(out, "* %x{}", reg)?;
            }
            writeln!(out, ")")?;
            for (ret, ptr) in returns.iter().zip(ret_regs) {
                let reg = context.push(*ret);
                write!(out, "  %x{reg} = load ")?;
                generate_type(out, *ret)?;
                write!(out, ", ")?;
                generate_type(out, *ret)?;
                writeln!(out, "* %x{ptr}")?
            }
            return Ok(());
        }
    }
    unreachable!()
}

fn get_sig(context: &Context, signature: &typecheck::Signature) -> Option<(Vec<Type>, Vec<Type>)> {
    let bindings = get_bindings(context, signature)?;
    Some((
        signature
            .params
            .iter()
            .map(|param| param.substitute_(&bindings))
            .collect(),
        signature
            .returns
            .iter()
            .map(|param| param.substitute_(&bindings))
            .collect(),
    ))
}

fn get_bindings(context: &Context, signature: &typecheck::Signature) -> Option<Vec<Type>> {
    let mut bindings = vec![None; signature.params.len()];
    for ((_, arg), param) in context
        .stack
        .iter()
        .rev()
        .zip(signature.params.iter().rev())
    {
        match param.bind_(*arg) {
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

fn generate_expr(
    out: &mut dyn Write,
    expr: &Expr,
    context: &mut Context,
    info: &TypeInfo,
) -> io::Result<()> {
    todo!()
}

fn generate_builtin(out: &mut dyn Write, name: &str, signature: &Signature) -> io::Result<()> {
    Ok(()) // todo!()
}

fn generate_auto(
    out: &mut dyn Write,
    name: &str,
    signature: &Signature,
    info: &TypeInfo,
) -> io::Result<()> {
    todo!()
}

fn mangled_name(out: &mut dyn Write, name: &str, params: &[Type]) -> io::Result<()> {
    if name == "main" {
        write!(out, "main")?;
    } else {
        escape(out, name)?;
        for param in params {
            write!(out, ".")?;
            mangled_type(out, *param)?;
        }
    }
    Ok(())
}

fn escape(out: &mut dyn Write, name: &str) -> io::Result<()> {
    for c in name.chars() {
        if c.is_ascii_alphabetic() && c != 'd' {
            write!(out, "{}", c)?;
        } else {
            write!(out, "${:x}$", c as u32)?;
        }
    }
    Ok(())
}

fn mangled_type(out: &mut dyn Write, ty: Type) -> io::Result<()> {
    match ty {
        Type::Custom(name, depth) => {
            write!(out, "{}", ".".repeat(depth))?;
            escape(out, name)
        }
        Type::Byte(depth) => {
            write!(out, "{}byte", ".".repeat(depth))
        }
        Type::Int(depth) => {
            write!(out, "{}int", ".".repeat(depth))
        }
        Type::Float(depth) => {
            write!(out, "{}float", ".".repeat(depth))
        }
        Type::Bool(depth) => {
            write!(out, "{}bool", ".".repeat(depth))
        }
    }
}

fn params(out: &mut dyn Write, signature: &Signature) -> io::Result<()> {
    write!(out, "(")?;
    for (i, param) in signature.params.iter().enumerate() {
        if i > 0 {
            write!(out, ", ")?;
        }
        generate_type(out, *param)?;
        write!(out, " %x{}", i)?;
    }
    for (i, ret) in signature.returns.iter().enumerate() {
        if i > 0 || !signature.params.is_empty() {
            write!(out, ", ")?;
        }
        generate_type(out, *ret)?;
        write!(out, "* %o{}", i)?;
    }
    write!(out, ")")
}

fn generate_type(out: &mut dyn Write, ty: Type) -> io::Result<()> {
    match ty {
        Type::Custom(name, depth) => {
            write!(out, "%")?;
            escape(out, name)?;
            write!(out, "{}", "*".repeat(depth))
        }
        Type::Byte(depth) => write!(out, "i8{}", "*".repeat(depth)),
        Type::Int(depth) => write!(out, "i64{}", "*".repeat(depth)),
        Type::Float(depth) => write!(out, "double{}", "*".repeat(depth)),
        Type::Bool(depth) => write!(out, "i8{}", "*".repeat(depth)),
    }
}

fn fill(
    name: &'static str,
    id: usize,
    binds: Vec<Type>,
    info: &TypeInfo,
    concrete: &mut Concrete,
    done: &mut Done,
) {
    let tup = dbg!((name, id, binds));
    if done.contains(&tup) {
        return;
    }
    let (_, _, binds) = tup;
    done.insert((name, id, binds.clone()));

    let signature = Signature::convert(&info.signatures.get(name).unwrap()[id], &binds);

    match concrete.entry(name) {
        hash_map::Entry::Vacant(v) => {
            v.insert(BTreeMap::from([(id, vec![signature])]));
        }
        hash_map::Entry::Occupied(mut o) => match o.get_mut().entry(id) {
            btree_map::Entry::Vacant(v) => {
                v.insert(vec![signature]);
            }
            btree_map::Entry::Occupied(mut o) => o.get_mut().push(signature),
        },
    }

    for (call_name, call_id, call_binds) in info.graph.get(&(name, id)).into_iter().flatten() {
        let converted_binds: Vec<_> = call_binds
            .iter()
            .map(|ty| Type::convert(ty, &binds))
            .collect();
        fill(call_name, *call_id, converted_binds, info, concrete, done);
    }
}

pub struct Signature {
    params: Vec<Type>,
    returns: Vec<Type>,
}

impl fmt::Debug for Signature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} -> {:?}", self.params, self.returns)
    }
}

impl Signature {
    fn convert(signature: &typecheck::Signature, binds: &[Type]) -> Signature {
        Signature {
            params: signature
                .params
                .iter()
                .map(|ty| Type::convert(ty, binds))
                .collect(),
            returns: signature
                .returns
                .iter()
                .map(|ty| Type::convert(ty, binds))
                .collect(),
        }
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub enum Type {
    Custom(&'static str, usize),
    Byte(usize),
    Int(usize),
    Float(usize),
    Bool(usize),
}

impl Type {
    pub fn inc(self) -> Type {
        match self {
            Type::Custom(name, depth) => Type::Custom(name, depth + 1),
            Type::Byte(depth) => Type::Byte(depth + 1),
            Type::Int(depth) => Type::Int(depth + 1),
            Type::Float(depth) => Type::Float(depth + 1),
            Type::Bool(depth) => Type::Bool(depth + 1),
        }
    }

    fn convert(ty: &typecheck::Type, binds: &[Type]) -> Type {
        match ty {
            typecheck::Type::Primitive(prim) => match prim {
                typecheck::Primitive::Int => Type::Int(0),
                typecheck::Primitive::Float => Type::Float(0),
                typecheck::Primitive::Byte => Type::Byte(0),
                typecheck::Primitive::Bool => Type::Bool(0),
            },
            typecheck::Type::Pointer(ty) => Type::convert(&*ty, binds).inc(),
            typecheck::Type::Custom(name) => Type::Custom(name.name, 0),
            typecheck::Type::Generic(index) => binds[*index],
        }
    }
}

use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use std::mem;

use crate::ast::*;
use crate::builtins::{Builtins, BUILTINS};
use crate::error::{Error, Note, BLUE, GREEN, RESET};
use crate::lex::Span;
use crate::program::*;
use crate::types::*;

pub fn check(unit: &[Item]) -> Result<Program, Error> {
    let mut context = Context::new(&BUILTINS);
    context.check_unit(unit)?;
    Ok(context.program_context.program)
}

pub struct Context {
    types: Types,
    signatures: HashMap<&'static str, Vec<GSignature>>,
    struct_fields: HashMap<&'static str, Vec<(&'static str, GTypeId)>>,
    let_binds: Vec<HashMap<&'static str, (usize, GTypeId)>>,
    call_graph: CallGraph,
    program_context: ProgramContext,
}

impl Context {
    fn new(builtins: &Builtins) -> Context {
        let mut types = Types::new();
        let signatures = builtins
            .iter()
            .map(|(name, sigs)| {
                (
                    *name,
                    sigs.iter()
                        .map(|(params, returns)| {
                            GSignature::new(
                                params
                                    .iter()
                                    .map(|p| types.get_or_insert(p.clone()))
                                    .collect(),
                                returns
                                    .iter()
                                    .map(|r| types.get_or_insert(r.clone()))
                                    .collect(),
                                Kind::Builtin,
                            )
                        })
                        .collect(),
                )
            })
            .collect();
        Context {
            types,
            signatures,
            struct_fields: HashMap::new(),
            let_binds: Vec::new(),
            call_graph: CallGraph::new(),
            program_context: ProgramContext::new(),
        }
    }

    fn init_signatures(&mut self, unit: &[Item]) -> Result<(), Error> {
        for item in unit {
            for (name, signature) in self.get_signatures(item)? {
                self.insert_signature(name, signature)?;
            }
        }
        Ok(())
    }

    fn check_unit(&mut self, unit: &[Item]) -> Result<(), Error> {
        self.types.init_generic_counts(unit)?;
        self.init_signatures(unit)?;
        for item in unit {
            self.check_item(item)?;
        }
        Ok(())
    }

    fn check_item(&mut self, item: &Item) -> Result<(), Error> {
        match item {
            Item::Function {
                name,
                generics,
                params,
                returns,
                body,
                rbrace_span,
                ..
            } => {
                let empty = Vec::new();
                let generic_names = generics.as_ref().map(|g| &g.names).unwrap_or(&empty);
                let params = params
                    .iter()
                    .map(|ty| self.types.convert(ty, generic_names))
                    .collect::<Result<Vec<_>, _>>()?;
                let returns = returns
                    .iter()
                    .map(|ty| self.types.convert(ty, generic_names))
                    .collect::<Result<Vec<_>, _>>()?;
                let index = self
                    .signatures
                    .get(name.name)
                    .unwrap()
                    .iter()
                    .position(|sig| sig.params == params)
                    .unwrap();
                self.call_graph.set_active(name.name, index);
                let mut stack: Vec<_> = params.iter().copied().enumerate().collect();
                self.begin_fn(name.name, generic_names.len(), params);
                for stmt in body {
                    self.check_stmt(&mut stack, stmt)?;
                }
                let stack_types: Vec<_> = stack.iter().map(|(_, ty)| *ty).collect();
                if stack_types == returns {
                    self.end_fn(stack);
                    Ok(())
                } else {
                    Err(Error::Type(
                        *rbrace_span,
                        "stack does not match the declared return type".to_string(),
                        vec![
                            Note::new(
                                None,
                                format!(
                                    "expected {}{:?}{}",
                                    BLUE.as_str(),
                                    self.types.format_types(&returns),
                                    RESET.as_str(),
                                ),
                            ),
                            Note::new(
                                None,
                                format!(
                                    "found {}{:?}{}",
                                    BLUE.as_str(),
                                    self.types.format_types(&stack_types),
                                    RESET.as_str(),
                                ),
                            ),
                        ],
                    ))
                }
            }
            Item::Struct { name, generics, .. } => {
                let mut members = Vec::new();
                for (name, type_id) in self.struct_fields.get(name.name).unwrap() {
                    members.push(ProgramMember {
                        name,
                        type_id: *type_id,
                    })
                }
                self.program_context.program.structs.push(ProgramStruct {
                    generic_count: generics.as_ref().map(|g| g.names.len()).unwrap_or(0),
                    name: name.name,
                    members,
                });
                Ok(())
            }
            Item::Global {
                name,
                ty,
                definition,
                ..
            } => {
                let ty = self.types.convert(ty, &[])?;
                let substituted = self.types.substitute_concrete(ty, &[]);
                self.begin_global(name.name, substituted);
                if let Some(definition) = definition {
                    self.begin_global_init();
                    let mut stack = Vec::new();
                    for op in &definition.group {
                        self.check_op(&mut stack, op)?;
                    }
                    if stack.len() == 1 && stack[0].1 == ty {
                        self.end_global(Some(stack[0].0));
                        Ok(())
                    } else {
                        let stack_types: Vec<_> = stack.iter().map(|(_, ty)| *ty).collect();
                        Err(Error::Type(
                            definition.rbrace_span,
                            "stack does not match declared type".to_string(),
                            vec![
                                Note::new(
                                    None,
                                    format!(
                                        "declared type is {}{}{}",
                                        BLUE.as_str(),
                                        self.types.format(ty),
                                        RESET.as_str(),
                                    ),
                                ),
                                Note::new(
                                    None,
                                    format!(
                                        "stack is {}{}{}",
                                        BLUE.as_str(),
                                        self.types.format_types(&stack_types),
                                        RESET.as_str(),
                                    ),
                                ),
                            ],
                        ))
                    }
                } else {
                    self.end_global(None);
                    Ok(())
                }
            }
            Item::Import { .. } => Ok(()),
        }
    }

    fn check_stmt(&mut self, stack: &mut Vec<(usize, GTypeId)>, stmt: &Stmt) -> Result<(), Error> {
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
                if_span,
                lbrace_span,
                rbrace_span,
                else_part,
                is_else_if,
            } => todo!(),
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
                ..
            } => self.check_let(stack, names, body, *let_span),
        }
    }

    fn check_let(
        &mut self,
        stack: &mut Vec<(usize, GTypeId)>,
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
                    stack.len(),
                ),
                vec![Note::new(
                    None,
                    format!(
                        "stack is {}{}{}",
                        BLUE.as_str(),
                        self.types.format_stack(stack),
                        RESET.as_str(),
                    ),
                )],
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

    fn check_op(&mut self, stack: &mut Vec<(usize, GTypeId)>, op: &Op) -> Result<(), Error> {
        macro_rules! prim {
            ($val:expr, $ty:ident, $name:ident) => {{
                let ty = self.types.$ty();
                let var = self.program_context.alloc(ty);
                self.push_op(ProgramOp::$name(var, $val));
                stack.push((var, ty));
                Ok(())
            }};
        }
        match op {
            Op::Int(val, _) => prim!(*val, int, Int),
            Op::Float(val, _) => prim!(*val, float, Float),
            Op::Bool(val, _) => prim!(*val, bool, Bool),
            Op::Char(val, _) => prim!(*val as i64, int, Int),
            Op::String(val, _) => prim!(val.clone(), byte_ptr, String),
            Op::Name(name) => self.check_name(stack, name),
            Op::Expr(expr, _) => {
                self.check_expr(expr)?;
                let ops = flatten_expr(expr);
                for op in &ops {
                    self.check_op(stack, op)?;
                }
                Ok(())
            }
        }
    }

    fn check_name(&mut self, stack: &mut Vec<(usize, GTypeId)>, name: &Name) -> Result<(), Error> {
        if name.name == "DEBUG_STACK" {
            println!(
                "{}DEBUG_STACK{} {}{}{} ({})",
                GREEN.as_str(),
                RESET.as_str(),
                BLUE.as_str(),
                self.types.format_stack(stack),
                RESET.as_str(),
                name.span.location(),
            );
        }

        if let Some((var, ty)) = self.get_let_bind(name.name) {
            stack.push((var, ty));
            return Ok(());
        }

        let (signature, bindings, index) = self.get_sbi(stack, name)?;
        let mut param_vars = Vec::new();
        for _ in 0..signature.params.len() {
            let (var, ty) = stack.pop().unwrap();
            self.program_context.free(var, ty);
            param_vars.push(var);
        }
        let mut return_vars = Vec::new();
        for ret in signature.returns {
            let ty = self.types.substitute(ret, &bindings);
            let var = self.program_context.alloc(ty);
            stack.push((var, ty));
            return_vars.push(var);
        }
        self.push_op(ProgramOp::Call(name.name, index, param_vars, return_vars));
        Ok(())
    }

    fn get_let_bind(&self, name: &'static str) -> Option<(usize, GTypeId)> {
        for let_bind in self.let_binds.iter().rev() {
            if let Some(var_ty) = let_bind.get(name) {
                return Some(*var_ty);
            }
        }
        None
    }

    fn get_sbi(
        &mut self,
        stack: &[(usize, GTypeId)],
        name: &Name,
    ) -> Result<(GSignature, Vec<GTypeId>, usize), Error> {
        if let Some(signatures) = self.signatures.get(name.name) {
            for (index, signature) in signatures.iter().enumerate() {
                if let Some(binds) = get_binds(&mut self.types, stack, signature) {
                    self.call_graph.insert(name.name, index, binds.clone());
                    return Ok((signature.clone(), binds, index));
                }
            }
            let mut notes = vec![Note::new(
                None,
                format!(
                    "stack is {}{}{}",
                    BLUE.as_str(),
                    self.types.format_stack(stack),
                    RESET.as_str()
                ),
            )];
            for signature in signatures {
                notes.push(Note::new(
                    None,
                    format!(
                        "'{}' has signature {}",
                        name.name,
                        self.types.format_signature(signature)
                    ),
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

    fn push_op(&mut self, op: ProgramOp) {
        if let Some(fn_wip) = &mut self.program_context.wip_fn {
            fn_wip.body.push(ProgramStmt::Op(op));
        } else {
            let global_wip = self.program_context.wip_global.as_mut().unwrap();
            global_wip.init.as_mut().unwrap().ops.push(op);
        }
    }

    fn check_expr(&mut self, expr: &Expr) -> Result<(), Error> {
        todo!()
    }

    fn begin_fn(&mut self, name: &'static str, generic_count: usize, params: Vec<GTypeId>) {
        self.program_context.init_vars(&params);
        self.program_context.wip_fn = Some(ProgramFn {
            name,
            generic_count,
            params,
            returns: Vec::new(),
            body: Vec::new(),
            vars: HashMap::new(),
            return_vars: Vec::new(),
        });
    }

    fn end_fn(&mut self, stack: Vec<(usize, GTypeId)>) {
        let mut function = self.program_context.wip_fn.take().unwrap();
        function.vars = mem::take(&mut self.program_context.vars);
        for (var, id) in stack {
            function.return_vars.push(var);
            function.returns.push(id);
        }
        self.program_context.program.functions.push(function);
    }

    fn begin_global(&mut self, name: &'static str, type_id: TypeId) {
        self.call_graph.set_active("main", 0);
        self.program_context.wip_global = Some(ProgramGlobal {
            name,
            type_id,
            init: None,
        })
    }

    fn begin_global_init(&mut self) {
        self.program_context.init_vars(&[]);
        self.program_context.wip_global.as_mut().unwrap().init = Some(ProgramGlobalInit {
            ops: Vec::new(),
            var: 0,
            vars: HashMap::new(),
        });
    }

    fn end_global(&mut self, var: Option<usize>) {
        let mut global = self.program_context.wip_global.take().unwrap();
        if let Some(init) = &mut global.init {
            init.var = var.unwrap();
            init.vars = mem::take(&mut self.program_context.vars);
        }
        self.program_context.program.globals.push(global);
    }

    fn get_signatures(&mut self, item: &Item) -> Result<Vec<(&'static str, GSignature)>, Error> {
        match item {
            Item::Function {
                name,
                generics,
                params,
                returns,
                ..
            } => {
                let empty = Vec::new();
                let generic_names = match generics {
                    Some(gen) => &gen.names,
                    None => &empty,
                };
                let params = params
                    .iter()
                    .map(|ty| self.types.convert(ty, generic_names))
                    .collect::<Result<Vec<_>, _>>()?;
                let returns = returns
                    .iter()
                    .map(|ty| self.types.convert(ty, generic_names))
                    .collect::<Result<_, _>>()?;

                let mut seen = HashSet::new();
                for ty in &params {
                    seen.extend(self.types.generic_indices(*ty));
                }
                for (i, name) in generic_names.iter().enumerate() {
                    if !seen.contains(&i) {
                        return Err(Error::Parse(
                            name.span,
                            format!("generic argument '{}' is never used", name.name),
                        ));
                    }
                }

                Ok(vec![(
                    name.name,
                    GSignature::new(params, returns, Kind::Custom(name.span)),
                )])
            }
            Item::Struct {
                name,
                generics,
                fields,
                ..
            } => {
                let empty = Vec::new();
                let generic_names = match generics {
                    Some(gen) => &gen.names,
                    None => &empty,
                };
                let ptype = PType {
                    stars: None,
                    name: *name,
                    generics: generics.as_ref().map(|generics| TypeGenerics {
                        types: generics
                            .names
                            .iter()
                            .map(|n| PType {
                                stars: None,
                                name: *n,
                                generics: None,
                            })
                            .collect(),
                        lbrack_span: Span::empty(),
                        rbrack_span: Span::empty(),
                    }),
                };
                let struct_ty = self.types.convert(&ptype, generic_names)?;
                let struct_ty_ptr = self.types.ref_n(struct_ty, 1);
                let mut constructor_params = Vec::new();
                let mut signatures = Vec::new();
                let mut struct_fields = Vec::new();
                let mut generic_indices = HashSet::new();
                for Field {
                    name: fname, ty, ..
                } in fields
                {
                    let ty = self.types.convert(ty, generic_names)?;
                    constructor_params.push(ty);
                    struct_fields.push((fname.name, ty));
                    generic_indices.extend(self.types.generic_indices(ty));
                    let ty_ptr = self.types.ref_n(ty, 1);
                    let dotdot = leak(format!("..{}", fname.name));
                    let dot = dotdot.strip_prefix('.').unwrap();
                    signatures.push((
                        dot,
                        GSignature::new(vec![struct_ty], vec![ty], Kind::Member(name.span)),
                    ));
                    signatures.push((
                        dotdot,
                        GSignature::new(
                            vec![struct_ty],
                            vec![struct_ty, ty],
                            Kind::Member(name.span),
                        ),
                    ));
                    signatures.push((
                        dot,
                        GSignature::new(vec![struct_ty_ptr], vec![ty_ptr], Kind::Member(name.span)),
                    ));
                    signatures.push((
                        dotdot,
                        GSignature::new(
                            vec![struct_ty_ptr],
                            vec![struct_ty_ptr, ty_ptr],
                            Kind::Member(name.span),
                        ),
                    ));
                }

                for (i, name) in generic_names.iter().enumerate() {
                    if !generic_indices.contains(&i) {
                        return Err(Error::Parse(
                            name.span,
                            format!("generic argument '{}' is never used", name.name),
                        ));
                    }
                }

                signatures.push((
                    name.name,
                    GSignature::new(
                        constructor_params,
                        vec![struct_ty],
                        Kind::Constructor(name.span),
                    ),
                ));
                self.struct_fields.insert(name.name, struct_fields);
                Ok(signatures)
            }
            Item::Global { name, ty, .. } => {
                let ty = self.types.convert(ty, &[])?;
                let write_name_ptr = leak(format!("write_{}_ptr", name.name));
                let write_name = write_name_ptr.strip_suffix("_ptr").unwrap();
                let name_ptr = write_name_ptr.strip_prefix("write_").unwrap();
                Ok(vec![
                    (
                        name.name,
                        GSignature::new(vec![], vec![ty], Kind::Global(name.span)),
                    ),
                    (
                        write_name,
                        GSignature::new(vec![ty], vec![], Kind::GlobalWrite(name.span)),
                    ),
                    (
                        name_ptr,
                        GSignature::new(
                            vec![],
                            vec![self.types.ref_n(ty, 1)],
                            Kind::GlobalPtr(name.span),
                        ),
                    ),
                ])
            }
            Item::Import { .. } => Ok(Vec::new()),
        }
    }

    fn insert_signature(&mut self, name: &'static str, signature: GSignature) -> Result<(), Error> {
        match self.signatures.entry(name) {
            Entry::Occupied(mut o) => {
                check_for_conflicts(&mut self.types, o.get(), &signature)?;
                o.get_mut().push(signature);
                Ok(())
            }
            Entry::Vacant(v) => {
                v.insert(vec![signature]);
                Ok(())
            }
        }
    }
}

pub type FnInfo = (&'static str, usize);
pub type CallInfo = (&'static str, usize, Vec<GTypeId>);

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

    fn set_active(&mut self, name: &'static str, index: usize) {
        self.active = (name, index);
    }

    fn insert(&mut self, name: &'static str, index: usize, bindings: Vec<GTypeId>) {
        if let Some(set) = self.graph.get_mut(&self.active) {
            set.insert((name, index, bindings));
        } else {
            let set = HashSet::from([(name, index, bindings)]);
            self.graph.insert(self.active, set);
        }
    }
}

fn get_binds(
    types: &mut Types,
    stack: &[(usize, GTypeId)],
    signature: &GSignature,
) -> Option<Vec<GTypeId>> {
    if signature.params.len() > stack.len() {
        None
    } else {
        let mut binds = vec![None; signature.params.len()];
        for ((_, arg), param) in stack.iter().rev().zip(signature.params.iter().rev()) {
            match types.bind(*param, *arg) {
                Ok(map) => {
                    for (index, ty) in map {
                        if let Some(bound) = &binds[index] {
                            if bound != &ty {
                                return None;
                            }
                        } else {
                            binds[index] = Some(ty);
                        }
                    }
                }
                Err(_) => return None,
            }
        }
        Some(binds.into_iter().flatten().collect())
    }
}

fn flatten_expr(expr: &Expr) -> Vec<Op> {
    todo!()
}

fn leak(s: String) -> &'static str {
    Box::leak(Box::new(s))
}

fn check_for_conflicts(
    types: &mut Types,
    existing_signatures: &[GSignature],
    new_signature: &GSignature,
) -> Result<(), Error> {
    for existing in existing_signatures {
        if params_overlap(types, &existing.params, &new_signature.params) {
            return Err(Error::Type(
                new_signature.kind.span(),
                "signature conflicts with a previous definition".to_string(),
                vec![],
                // TODO: include Note with existing_signature's span
                // TOOD: include Note with existing_signature and new_signature
            ));
        }
    }
    Ok(())
}

fn params_overlap(types: &mut Types, params_a: &[GTypeId], params_b: &[GTypeId]) -> bool {
    let a_rev = params_a.iter().rev();
    let b_rev = params_b.iter().rev();
    a_rev.zip(b_rev).all(|(a, b)| types.overlap(*a, *b))
}

pub struct ProgramInfo {
    program: Program,
    call_graph: CallGraph,
    types: Types,
}

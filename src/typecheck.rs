use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use std::mem;

use petgraph::algo::toposort;
use petgraph::graph::Graph;

use crate::ast::*;
use crate::builtins::{Builtins, BUILTINS};
use crate::error::{Error, Note};
use crate::lex::Span;
use crate::program::*;
use crate::types::*;
use crate::{color, reset};

// TODO: topological sort on global uses in global inits (check
//       call graph to find dependencies)

// TODO: is the resolve step required? (maybe the way we allocate
//       stack variables means that step is redundant)

pub fn check(unit: &[Item]) -> Result<ProgramInfo, Error> {
    let mut context = Context::new(&BUILTINS);
    context.check_unit(unit)?;
    Ok(ProgramInfo {
        program: context.program_context.program,
        call_graph: context.call_graph.graph,
        signatures: context.signatures,
        struct_fields: context.struct_fields,
        types: context.types,
    })
}

pub struct Context {
    types: Types,
    signatures: HashMap<&'static str, Vec<GSignature>>,
    struct_fields: HashMap<&'static str, Vec<(&'static str, GTypeId)>>,
    let_binds: Vec<HashMap<&'static str, (usize, GTypeId)>>,
    call_graph: CallGraph,
    globals: HashMap<&'static str, GTypeId>,
    generic_names: Vec<Name>,
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
            globals: HashMap::new(),
            generic_names: Vec::new(),
            program_context: ProgramContext::new(),
        }
    }

    fn init_signatures(&mut self, unit: &[Item]) -> Result<(), Error> {
        for item in unit {
            for (name, signature) in self.get_signatures(item)? {
                self.insert_signature(name, signature)?;
            }
        }
        self.check_for_recursive_struct_defs(unit)?;
        if let Some(mains) = self.signatures.get("main") {
            if mains.len() == 1 {
                let GSignature {
                    params,
                    returns,
                    kind,
                } = mains[0].clone();

                if !params.is_empty() && params != [self.types.int(), self.types.byte_ptr_ptr()] {
                    Err(Error::Type(
                        kind.span().unwrap(),
                        format!(
                            "'main' must have parameters {}[]{} or {}[int, **byte]{}",
                            color!(Blue),
                            reset!(),
                            color!(Blue),
                            reset!(),
                        ),
                        vec![Note::new(
                            None,
                            format!(
                                "'main' has signature {}{} -> {}{}",
                                color!(Blue),
                                self.types.format_types(&params),
                                self.types.format_types(&returns),
                                reset!(),
                            ),
                        )],
                    ))
                } else if !returns.is_empty() && returns != [self.types.int()] {
                    Err(Error::Type(
                        kind.span().unwrap(),
                        format!(
                            "'main' must return {}[]{} or {}[int]{}",
                            color!(Blue),
                            reset!(),
                            color!(Blue),
                            reset!(),
                        ),
                        vec![Note::new(
                            None,
                            format!(
                                "'main' has signature {}{} -> {}{}",
                                color!(Blue),
                                self.types.format_types(&params),
                                self.types.format_types(&returns),
                                reset!(),
                            ),
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
                        .map(|s| {
                            Note::new(
                                Some(s.kind.span().unwrap()),
                                "'main' defined here".to_string(),
                            )
                        })
                        .collect(),
                ))
            }
        } else {
            Err(Error::Main("no function named 'main'".to_string(), vec![]))
        }
    }

    fn check_for_recursive_struct_defs(&self, unit: &[Item]) -> Result<(), Error> {
        let mut graph = Graph::new();
        let mut map = HashMap::new();
        macro_rules! get_id {
            ($name:expr) => {
                if let Some(id) = map.get($name) {
                    *id
                } else {
                    let id = graph.add_node($name);
                    map.insert($name, id);
                    id
                }
            };
        }
        for (name, fields) in &self.struct_fields {
            let name_id = get_id!(*name);
            for (_, ty) in fields {
                if let Some(cname) = self.types.nonptr_custom_name(*ty) {
                    let cname_id = get_id!(cname);
                    graph.add_edge(name_id, cname_id, ());
                }
            }
        }
        if let Err(start_id) = toposort(&graph, None).map_err(|c| c.node_id()) {
            let mut stack = vec![start_id];
            let mut prev = HashMap::new();
            while let Some(mut id) = stack.pop() {
                for neighbor_id in graph.neighbors(id) {
                    if neighbor_id == start_id {
                        let mut names = vec![*graph.node_weight(id).unwrap()];
                        while let Some(prev_id) = prev.get(&id) {
                            id = *prev_id;
                            names.insert(0, *graph.node_weight(id).unwrap());
                        }
                        let name_set: HashSet<_> = names.iter().collect();
                        let mut items = unit.iter();
                        let span = loop {
                            match items.next().unwrap() {
                                Item::Struct { name, .. } if name_set.contains(&name.name) => {
                                    break name.span;
                                }
                                _ => {}
                            }
                        };
                        let mut rot = 0;
                        for (i, name) in names.iter().enumerate() {
                            if *name == span.text {
                                rot = i;
                                break;
                            }
                        }
                        let mut front: Vec<_> = names.drain(..rot).collect();
                        names.append(&mut front);
                        let msg = match names.len() {
                            1 => format!("struct '{}' is defined recursively", names[0]),
                            2 => format!(
                                "structs '{}' and '{}' are defined recursively",
                                names[0], names[1]
                            ),
                            _ => {
                                let mut msg = "structs ".to_string();
                                #[allow(clippy::needless_range_loop)]
                                for i in 0..(names.len() - 1) {
                                    if i > 0 {
                                        msg.push_str(", '");
                                    } else {
                                        msg.push('\'');
                                    }
                                    msg.push_str(names[i]);
                                    msg.push('\'');
                                }
                                msg.push_str(" and '");
                                msg.push_str(names.last().unwrap());
                                msg.push_str("' are defined recursively");
                                msg
                            }
                        };
                        return Err(Error::Type(span, msg, vec![]));
                    } else {
                        prev.insert(neighbor_id, id);
                        stack.push(neighbor_id);
                    }
                }
            }
            unreachable!();
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
                self.generic_names = generics
                    .as_ref()
                    .map(|g| g.names.clone())
                    .unwrap_or_default();
                let params = params
                    .iter()
                    .map(|ty| self.types.convert(ty, &self.generic_names))
                    .collect::<Result<Vec<_>, _>>()?;
                let returns = returns
                    .iter()
                    .map(|ty| self.types.convert(ty, &self.generic_names))
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
                self.program_context.begin_fn(&params);
                for stmt in body {
                    self.check_stmt(&mut stack, stmt)?;
                }
                let stack_types: Vec<_> = stack.iter().map(|(_, ty)| *ty).collect();
                if stack_types == returns {
                    self.program_context.end_fn(
                        &stack,
                        name.name,
                        index,
                        self.generic_names.len(),
                        params,
                        returns,
                    );
                    Ok(())
                } else {
                    Err(Error::Type(
                        *rbrace_span,
                        "stack does not match the declared return type".to_string(),
                        vec![
                            Note::new(
                                None,
                                format!(
                                    "expected {}{}{}",
                                    color!(Blue),
                                    self.types.format_types(&returns),
                                    reset!(),
                                ),
                            ),
                            Note::new(
                                None,
                                format!(
                                    "found {}{}{}",
                                    color!(Blue),
                                    self.types.format_types(&stack_types),
                                    reset!(),
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
                self.program_context.begin_global();
                if let Some(definition) = definition {
                    self.call_graph.set_active("main", 0);
                    let mut stack = Vec::new();
                    for op in &definition.group {
                        self.check_op(&mut stack, op)?;
                    }
                    if stack.len() == 1 && stack[0].1 == ty {
                        self.program_context
                            .end_global_init(name.name, substituted, stack[0].0);
                        Ok(())
                    } else {
                        Err(Error::Type(
                            definition.rbrace_span,
                            "stack does not match declared type".to_string(),
                            vec![
                                Note::new(
                                    None,
                                    format!(
                                        "declared type is {}{}{}",
                                        color!(Blue),
                                        self.types.format(ty),
                                        reset!(),
                                    ),
                                ),
                                Note::new(
                                    None,
                                    format!(
                                        "stack is {}{}{}",
                                        color!(Blue),
                                        self.types.format_stack(&stack),
                                        reset!(),
                                    ),
                                ),
                            ],
                        ))
                    }
                } else {
                    self.program_context.end_global(name.name, substituted);
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
                lbrace_span,
                rbrace_span,
                else_part,
                is_else_if,
                ..
            } => self.check_if(
                stack,
                test,
                body,
                *lbrace_span,
                *rbrace_span,
                else_part,
                *is_else_if,
            ),
            Stmt::While {
                test,
                body,
                lbrace_span,
                rbrace_span,
                ..
            } => self.check_while(stack, test, body, *lbrace_span, *rbrace_span),
            Stmt::For {
                low,
                high,
                body,
                to_span,
                lbrace_span,
                rbrace_span,
                ..
            } => self.check_for(stack, low, high, body, *to_span, *lbrace_span, *rbrace_span),
            Stmt::Let {
                names,
                body,
                let_span,
                ..
            } => self.check_let(stack, names, body, *let_span),
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn check_if(
        &mut self,
        stack: &mut Vec<(usize, GTypeId)>,
        test: &[Op],
        body: &[Stmt],
        lbrace_span: Span,
        rbrace_span: Span,
        else_part: &Option<ElsePart>,
        is_else_if: bool,
    ) -> Result<(), Error> {
        for op in test {
            self.check_op(stack, op)?;
        }

        let bool_id = self.types.bool();
        if stack.is_empty() || stack.last().unwrap().1 != bool_id {
            return Err(Error::Type(
                lbrace_span,
                "expected a 'bool' for if test".to_string(),
                vec![Note::new(
                    None,
                    format!(
                        "stack is {}{}{}",
                        color!(Blue),
                        self.types.format_stack(stack),
                        reset!(),
                    ),
                )],
            ));
        }

        let test_var = stack.pop().unwrap().0;

        let stack_before = if is_else_if {
            format!(
                "before else if block, stack is {}{}{} ({})",
                color!(Blue),
                self.types.format_stack(stack),
                reset!(),
                lbrace_span.location(),
            )
        } else {
            format!(
                "before if block, stack is {}{}{} ({})",
                color!(Blue),
                self.types.format_stack(stack),
                reset!(),
                lbrace_span.location()
            )
        };

        let mut else_stack = stack.clone();

        self.program_context.begin_if();
        for stmt in body {
            self.check_stmt(stack, stmt)?;
        }

        self.program_context.begin_else(stack, &else_stack);
        if let Some(else_part) = else_part {
            for stmt in &else_part.body {
                self.check_stmt(&mut else_stack, stmt)?;
            }
        }

        if stack.len() != else_stack.len()
            || stack
                .iter()
                .zip(else_stack.iter())
                .any(|((_, s), (_, e))| s != e)
        {
            if let Some(else_part) = else_part {
                return Err(Error::Type(
                    else_part.rbrace_span,
                    if is_else_if {
                        "else if and else blocks have mismatched stacks".to_string()
                    } else {
                        "if and else blocks have mismatched stacks".to_string()
                    },
                    vec![
                        Note::new(None, stack_before),
                        if is_else_if {
                            Note::new(
                                None,
                                format!(
                                    "after else if block, stack is {}{}{} ({})",
                                    color!(Blue),
                                    self.types.format_stack(stack),
                                    reset!(),
                                    rbrace_span.location(),
                                ),
                            )
                        } else {
                            Note::new(
                                None,
                                format!(
                                    "after if block, stack is {}{}{} ({})",
                                    color!(Blue),
                                    self.types.format_stack(stack),
                                    reset!(),
                                    rbrace_span.location(),
                                ),
                            )
                        },
                        Note::new(
                            None,
                            format!(
                                "after else block, stack is {}{}{} ({})",
                                color!(Blue),
                                self.types.format_stack(&else_stack),
                                reset!(),
                                else_part.rbrace_span.location(),
                            ),
                        ),
                    ],
                ));
            } else {
                return Err(Error::Type(
                    rbrace_span,
                    if is_else_if {
                        "stack does not match the stack before else if block".to_string()
                    } else {
                        "stack does not match the stack before if block".to_string()
                    },
                    vec![
                        Note::new(None, stack_before),
                        if is_else_if {
                            Note::new(
                                None,
                                format!(
                                    "after else if block, stack is {}{}{} ({})",
                                    color!(Blue),
                                    self.types.format_stack(stack),
                                    reset!(),
                                    rbrace_span.location(),
                                ),
                            )
                        } else {
                            Note::new(
                                None,
                                format!(
                                    "after if block, stack is {}{}{} ({})",
                                    color!(Blue),
                                    self.types.format_stack(stack),
                                    reset!(),
                                    rbrace_span.location(),
                                ),
                            )
                        },
                    ],
                ));
            }
        }

        let mut resolve = Vec::new();
        for ((var, id), (else_var, _)) in stack.iter().copied().zip(else_stack) {
            if var != else_var {
                resolve.push((var, else_var, id));
            }
        }
        self.program_context.end_if_else(test_var, resolve);
        self.program_context.free(test_var, bool_id);

        Ok(())
    }

    fn check_while(
        &mut self,
        stack: &mut Vec<(usize, GTypeId)>,
        test: &[Op],
        body: &[Stmt],
        lbrace_span: Span,
        rbrace_span: Span,
    ) -> Result<(), Error> {
        let mut stack_before = stack.clone();

        for op in test {
            self.check_op(stack, op)?;
        }

        let bool_id = self.types.bool();
        if stack.is_empty() || stack.last().unwrap().1 != bool_id {
            return Err(Error::Type(
                lbrace_span,
                "expected a 'bool' for while test".to_string(),
                vec![Note::new(
                    None,
                    format!(
                        "stack is {}{}{}",
                        color!(Blue),
                        self.types.format_stack(stack),
                        reset!(),
                    ),
                )],
            ));
        }
        if stack.len() != stack_before.len() + 1
            || stack
                .iter()
                .zip(stack_before.iter())
                .any(|((_, s), (_, b))| s != b)
        {
            return Err(Error::Type(
                lbrace_span,
                "while test should add a single 'bool' to the stack".to_string(),
                vec![
                    Note::new(
                        None,
                        format!(
                            "before test, stack is {}{}{}",
                            color!(Blue),
                            self.types.format_stack(&stack_before),
                            reset!(),
                        ),
                    ),
                    Note::new(
                        None,
                        format!(
                            "after test, stack is {}{}{}",
                            color!(Blue),
                            self.types.format_stack(stack),
                            reset!(),
                        ),
                    ),
                ],
            ));
        }

        let test_var = stack.pop().unwrap().0;
        stack_before = stack.clone();

        self.program_context.begin_while();
        for stmt in body {
            self.check_stmt(stack, stmt)?;
        }

        if stack.len() != stack_before.len()
            || stack
                .iter()
                .zip(stack_before.iter())
                .any(|((_, s), (_, b))| s != b)
        {
            return Err(Error::Type(
                rbrace_span,
                "stack does not match the stack before while block".to_string(),
                vec![
                    Note::new(
                        None,
                        format!(
                            "before while block, stack is {}{}{}",
                            color!(Blue),
                            self.types.format_stack(&stack_before),
                            reset!(),
                        ),
                    ),
                    Note::new(
                        None,
                        format!(
                            "after while block, stack is {}{}{}",
                            color!(Blue),
                            self.types.format_stack(stack),
                            reset!(),
                        ),
                    ),
                ],
            ));
        }

        for op in test {
            self.check_op(stack, op).unwrap();
        }

        let mut resolve = Vec::new();
        let (t_var, bool_id) = stack.pop().unwrap();
        if t_var != test_var {
            resolve.push((test_var, t_var, bool_id));
        }
        for ((var_before, id), (var, _)) in stack_before.iter().copied().zip(stack.iter().copied())
        {
            if var_before != var {
                resolve.push((var_before, var, id));
            }
        }
        self.program_context.end_while(test_var, resolve);
        self.program_context.free(test_var, self.types.bool());

        mem::swap(stack, &mut stack_before);

        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    fn check_for(
        &mut self,
        stack: &mut Vec<(usize, GTypeId)>,
        low: &[Op],
        high: &[Op],
        body: &[Stmt],
        to_span: Span,
        lbrace_span: Span,
        rbrace_span: Span,
    ) -> Result<(), Error> {
        let stack_before = stack.clone();

        for op in low {
            self.check_op(stack, op)?;
        }

        let int_id = self.types.int();
        if stack.is_empty() || stack.last().unwrap().1 != int_id {
            return Err(Error::Type(
                to_span,
                "expected a 'int' in for lower bound".to_string(),
                vec![Note::new(
                    None,
                    format!(
                        "stack is {}{}{}",
                        color!(Blue),
                        self.types.format_stack(stack),
                        reset!(),
                    ),
                )],
            ));
        }
        if stack.len() != stack_before.len() + 1
            || stack
                .iter()
                .zip(stack_before.iter())
                .any(|((_, s), (_, b))| s != b)
        {
            return Err(Error::Type(
                to_span,
                "lower bound should add a single 'int' to the stack".to_string(),
                vec![
                    Note::new(
                        None,
                        format!(
                            "before bound, stack is {}{}{}",
                            color!(Blue),
                            self.types.format_stack(&stack_before),
                            reset!(),
                        ),
                    ),
                    Note::new(
                        None,
                        format!(
                            "after bound, stack is {}{}{}",
                            color!(Blue),
                            self.types.format_stack(stack),
                            reset!(),
                        ),
                    ),
                ],
            ));
        }

        let low_var = stack.pop().unwrap().0;

        for op in high {
            self.check_op(stack, op)?;
        }

        if stack.is_empty() || stack.last().unwrap().1 != int_id {
            return Err(Error::Type(
                lbrace_span,
                "expected a 'int' in for upper bound".to_string(),
                vec![Note::new(
                    None,
                    format!(
                        "stack is {}{}{}",
                        color!(Blue),
                        self.types.format_stack(stack),
                        reset!(),
                    ),
                )],
            ));
        }
        if stack.len() != stack_before.len() + 1
            || stack
                .iter()
                .zip(stack_before.iter())
                .any(|((_, s), (_, b))| s != b)
        {
            return Err(Error::Type(
                lbrace_span,
                "upper bound should add a single 'int' to the stack".to_string(),
                vec![
                    Note::new(
                        None,
                        format!(
                            "before bound, stack is {}{}{}",
                            color!(Blue),
                            self.types.format_stack(&stack_before),
                            reset!(),
                        ),
                    ),
                    Note::new(
                        None,
                        format!(
                            "after bound, stack is {}{}{}",
                            color!(Blue),
                            self.types.format_stack(stack),
                            reset!(),
                        ),
                    ),
                ],
            ));
        }

        let high_var = stack.pop().unwrap().0;
        let mut stack_before = stack.clone();
        let iter_var = self.program_context.alloc(int_id);
        stack.push((iter_var, int_id));
        self.program_context.for_vars.insert(iter_var);

        self.program_context.begin_for();
        for stmt in body {
            self.check_stmt(stack, stmt)?;
        }

        if stack.len() != stack_before.len()
            || stack
                .iter()
                .zip(stack_before.iter())
                .any(|((_, s), (_, b))| s != b)
        {
            return Err(Error::Type(
                rbrace_span,
                "stack does not match the stack before for block".to_string(),
                vec![
                    Note::new(
                        None,
                        format!(
                            "before for block, stack is {}{}{}",
                            color!(Blue),
                            self.types.format_stack(&stack_before),
                            reset!(),
                        ),
                    ),
                    Note::new(
                        None,
                        format!(
                            "after for block, stack is {}{}{}",
                            color!(Blue),
                            self.types.format_stack(stack),
                            reset!(),
                        ),
                    ),
                ],
            ));
        }

        let mut resolve = Vec::new();
        for ((var_before, id), (var, _)) in stack_before.iter().copied().zip(stack.iter().copied())
        {
            if var_before != var {
                resolve.push((var_before, var, id));
            }
        }
        self.program_context
            .end_for(iter_var, low_var, high_var, resolve);
        self.program_context.for_vars.remove(&iter_var);
        self.program_context.free(iter_var, int_id);
        self.program_context.free(low_var, int_id);
        self.program_context.free(high_var, int_id);

        mem::swap(stack, &mut stack_before);

        Ok(())
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
                        color!(Blue),
                        self.types.format_stack(stack),
                        reset!(),
                    ),
                )],
            ));
        }

        let mut binds = HashMap::new();
        for name in names.iter().rev() {
            let ty = stack.pop().unwrap();
            self.program_context.alloc_let(ty.0);
            if name.name != "_" {
                binds.insert(name.name, ty);
            }
        }
        self.let_binds.push(binds);

        for stmt in body {
            self.check_stmt(stack, stmt)?;
        }

        for (_, (var, ty)) in self.let_binds.pop().unwrap() {
            self.program_context.free_let(var, ty);
        }

        Ok(())
    }

    fn check_op(&mut self, stack: &mut Vec<(usize, GTypeId)>, op: &Op) -> Result<(), Error> {
        macro_rules! prim {
            ($val:expr, $ty:ident, $name:ident) => {{
                let ty = self.types.$ty();
                let var = self.program_context.alloc(ty);
                self.program_context.push_op(ProgramOp::$name(var, $val));
                stack.push((var, ty));
                Ok(())
            }};
        }

        match op {
            Op::Int(val, _) => prim!(*val, int, Int),
            Op::Float(val, _) => prim!(*val, float, Float),
            Op::Bool(val, _) => prim!(*val, bool, Bool),
            Op::Char(val, _) => prim!(*val as i64, int, Int),
            Op::Byte(val, _) => prim!(*val, byte, Byte),
            Op::String(val, _) => {
                let ty = self.types.byte_ptr();
                let var = self.program_context.alloc(ty);
                self.program_context
                    .push_op(ProgramOp::String(var, val.clone()));
                stack.push((var, ty));
                Ok(())
            }
            Op::Name(name) => self.check_name(stack, name),
            Op::Expr(expr, _) => {
                self.program_context.pause();
                self.check_expr(expr)?;
                self.program_context.resume();
                let mut ops = Vec::new();
                flatten_expr(expr, &mut ops);
                for op in &ops {
                    self.check_op(stack, op)?;
                }
                Ok(())
            }
            Op::SizeOf(ptype, _, _, _) => {
                let ty = self.types.convert(ptype, &self.generic_names)?;
                let int_id = self.types.int();
                let var = self.program_context.alloc(int_id);
                stack.push((var, int_id));
                self.program_context.push_op(ProgramOp::SizeOf(var, ty));
                Ok(())
            }
            Op::Alloc(ptype, _, _, _) => {
                let ty = self.types.convert(ptype, &self.generic_names)?;
                let ty_ptr = self.types.ref_n(ty, 1);
                let var = self.program_context.alloc(ty_ptr);
                stack.push((var, ty_ptr));
                self.program_context.push_op(ProgramOp::Alloc(var, ty));
                Ok(())
            }
            Op::Zalloc(ptype, _, _, _) => {
                let ty = self.types.convert(ptype, &self.generic_names)?;
                let ty_ptr = self.types.ref_n(ty, 1);
                let var = self.program_context.alloc(ty_ptr);
                stack.push((var, ty_ptr));
                self.program_context.push_op(ProgramOp::Zalloc(var, ty));
                Ok(())
            }
            Op::AllocArr(ptype, span, _, _) => {
                if stack.is_empty() || stack.last().unwrap().1 != self.types.int() {
                    return Err(Error::Type(
                        *span,
                        "'alloc_arr' expects an 'int'".to_string(),
                        vec![Note::new(
                            None,
                            format!(
                                "stack is {}{}{}",
                                color!(Blue),
                                self.types.format_stack(stack),
                                reset!(),
                            ),
                        )],
                    ));
                }
                let len_var = stack.pop().unwrap().0;
                let ty = self.types.convert(ptype, &self.generic_names)?;
                let ty_ptr = self.types.ref_n(ty, 1);
                let var = self.program_context.alloc(ty_ptr);
                stack.push((var, ty_ptr));
                self.program_context
                    .push_op(ProgramOp::AllocArr(var, len_var, ty));
                Ok(())
            }
            Op::ZallocArr(ptype, span, _, _) => {
                if stack.is_empty() || stack.last().unwrap().1 != self.types.int() {
                    return Err(Error::Type(
                        *span,
                        "'zalloc_arr' expects an 'int'".to_string(),
                        vec![Note::new(
                            None,
                            format!(
                                "stack is {}{}{}",
                                color!(Blue),
                                self.types.format_stack(stack),
                                reset!(),
                            ),
                        )],
                    ));
                }
                let len_var = stack.pop().unwrap().0;
                let ty = self.types.convert(ptype, &self.generic_names)?;
                let ty_ptr = self.types.ref_n(ty, 1);
                let var = self.program_context.alloc(ty_ptr);
                stack.push((var, ty_ptr));
                self.program_context
                    .push_op(ProgramOp::ZallocArr(var, len_var, ty));
                Ok(())
            }
            Op::CastTo(ptype, span, _, _) => {
                if stack.is_empty()
                    || (self.types.depth(stack.last().unwrap().1) == 0
                        && stack.last().unwrap().1 != self.types.int())
                {
                    return Err(Error::Type(
                        *span,
                        "'cast_to' expects a pointer or 'int'".to_string(),
                        vec![Note::new(
                            None,
                            format!(
                                "stack is {}{}{}",
                                color!(Blue),
                                self.types.format_stack(stack),
                                reset!(),
                            ),
                        )],
                    ));
                }
                let arg_var = stack.pop().unwrap().0;
                let ty = self.types.convert(ptype, &self.generic_names)?;
                if self.types.depth(ty) == 0 && ty != self.types.int() {
                    return Err(Error::Type(
                        *span,
                        "cast target must be a pointer or 'int'".to_string(),
                        vec![Note::new(
                            None,
                            format!(
                                "cast target is {}{}{}",
                                color!(Blue),
                                self.types.format(ty),
                                reset!(),
                            ),
                        )],
                    ));
                }
                let var = self.program_context.alloc(ty);
                stack.push((var, ty));
                self.program_context
                    .push_op(ProgramOp::CastTo(var, arg_var, ty));
                Ok(())
            }
        }
    }

    fn check_name(&mut self, stack: &mut Vec<(usize, GTypeId)>, name: &Name) -> Result<(), Error> {
        if name.name == "DEBUG_STACK" {
            println!(
                "{}DEBUG_STACK{} {}{}{} ({})",
                color!(Green),
                reset!(),
                color!(Blue),
                self.types.format_stack(stack),
                reset!(),
                name.span.location(),
            );
        } else if name.name == "_" {
            return Err(Error::Parse(name.span, "name '_' is reserved".to_string()));
        }

        if let Some((var, ty)) = self.get_let_bind(name.name) {
            stack.push((var, ty));
            return Ok(());
        }

        let (signature, binds, index) = self.get_sbi(stack, name)?;
        let mut param_vars = Vec::new();
        for _ in 0..signature.params.len() {
            let (var, ty) = stack.pop().unwrap();
            self.program_context.free(var, ty);
            param_vars.push(var);
        }
        param_vars.reverse();
        let mut return_vars = Vec::new();
        for ret in signature.returns {
            let ty = self.types.substitute(ret, &binds);
            let var = self.program_context.alloc(ty);
            stack.push((var, ty));
            return_vars.push(var);
        }
        if name.name == "@" {
            self.program_context.push_op(ProgramOp::Ref(
                return_vars[1],
                param_vars[0],
                stack[stack.len() - 2].1,
            ));
        } else if name.name == "abort" {
            self.program_context.push_op(ProgramOp::Abort(name.span));
        } else if name.name == "assert" && *signature.params.last().unwrap() == self.types.bool() {
            self.program_context
                .push_op(ProgramOp::Assert(param_vars[0], name.span));
        } else {
            self.program_context.push_op(ProgramOp::Call(
                name.name,
                index,
                param_vars,
                return_vars,
                binds,
            ));
        }
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
                    color!(Blue),
                    self.types.format_stack(stack),
                    reset!()
                ),
            )];
            for signature in signatures {
                notes.push(Note::new(
                    None,
                    format!(
                        "'{}' has signature {}{}{}",
                        name.name,
                        color!(Blue),
                        self.types.format_signature(signature),
                        reset!(),
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

    fn check_expr(&mut self, expr: &Expr) -> Result<GTypeId, Error> {
        macro_rules! binop {
            ($op:expr, $left:ident, $right:ident, $span:expr) => {{
                let stack = vec![(0, self.check_expr($left)?), (0, self.check_expr($right)?)];
                let (signature, binds, _) = self.get_sbi(&stack, &$span.into())?;
                if signature.returns.len() == 1 {
                    Ok(self.types.substitute(signature.returns[0], &binds))
                } else {
                    Err(Error::Type(
                        $span,
                        "expression operators must return a single value".to_string(),
                        vec![
                            Note::new(
                                None,
                                format!("'{}' returns {} values", $op, signature.returns.len()),
                            ),
                            Note::new(
                                None,
                                format!(
                                    "'{}' has signature {}{}{}",
                                    $op,
                                    color!(Blue),
                                    self.types.format_signature(&signature),
                                    reset!(),
                                ),
                            ),
                        ],
                    ))
                }
            }};
        }

        macro_rules! uop {
            ($op:expr, $expr:ident, $span:expr) => {{
                let stack = vec![(0, self.check_expr($expr)?)];
                let (signature, binds, _) = self.get_sbi(&stack, &$span.into())?;
                if signature.returns.len() == 1 {
                    Ok(self.types.substitute(signature.returns[0], &binds))
                } else {
                    Err(Error::Type(
                        $span,
                        "expression operators must return a single value".to_string(),
                        vec![
                            Note::new(
                                None,
                                format!("'{}' returns {} values", $op, signature.returns.len()),
                            ),
                            Note::new(
                                None,
                                format!(
                                    "'{}' has signature {}{}{}",
                                    $op,
                                    color!(Blue),
                                    self.types.format_signature(&signature),
                                    reset!(),
                                ),
                            ),
                        ],
                    ))
                }
            }};
        }

        match expr {
            Expr::Int(_, _) => Ok(self.types.int()),
            Expr::Float(_, _) => Ok(self.types.float()),
            Expr::Bool(_, _) => Ok(self.types.bool()),
            Expr::Char(_, _) => Ok(self.types.int()),
            Expr::Byte(_, _) => Ok(self.types.byte()),
            Expr::String(_, _) => Ok(self.types.byte_ptr()),
            Expr::Name(name) => {
                if let Some((_, ty)) = self.get_let_bind(name.name) {
                    Ok(ty)
                } else if let Some(ty) = self.globals.get(name.name) {
                    Ok(*ty)
                } else {
                    Err(Error::Type(
                        name.span,
                        format!("variable '{}' does not exist", name.name),
                        vec![Note::new(
                            None,
                            "names in expressions must be let or global variables".to_string(),
                        )],
                    ))
                }
            }
            Expr::Add(left, right, span) => binop!("+", left, right, *span),
            Expr::And(left, right, span) => binop!("&", left, right, *span),
            Expr::Divide(left, right, span) => binop!("/", left, right, *span),
            Expr::Equal(left, right, span) => binop!("==", left, right, *span),
            Expr::GreaterEqual(left, right, span) => binop!(">=", left, right, *span),
            Expr::GreaterThan(left, right, span) => binop!(">", left, right, *span),
            Expr::LessEqual(left, right, span) => binop!("<=", left, right, *span),
            Expr::LessThan(left, right, span) => binop!("<", left, right, *span),
            Expr::Modulo(left, right, span) => binop!("%", left, right, *span),
            Expr::Multiply(left, right, span) => binop!("*", left, right, *span),
            Expr::NotEqual(left, right, span) => binop!("!=", left, right, *span),
            Expr::Or(left, right, span) => binop!("|", left, right, *span),
            Expr::Subtract(left, right, span) => binop!("-", left, right, *span),
            Expr::Xor(left, right, span) => binop!("^", left, right, *span),
            Expr::LShift(left, right, span) => binop!("<<", left, right, *span),
            Expr::RShift(left, right, span) => binop!(">>", left, right, *span),
            Expr::Not(expr, span) => uop!("!", expr, *span),
            Expr::Negate(expr, span) => {
                let stack = vec![(0, self.check_expr(expr)?)];
                let name = Name {
                    name: "neg",
                    span: *span,
                };
                let (signature, binds) = match self.get_sbi(&stack, &name) {
                    Ok((s, b, _)) => (s, b),
                    Err(mut err) => {
                        err.insert_note(Note::new(
                            None,
                            "unary operator '-' is translated to 'neg'".to_string(),
                        ));
                        return Err(err);
                    }
                };
                if signature.returns.len() == 1 {
                    Ok(self.types.substitute(signature.returns[0], &binds))
                } else {
                    Err(Error::Type(
                        *span,
                        "expression operators must return a single value".to_string(),
                        vec![
                            Note::new(
                                None,
                                "unary operator '-' is translated to 'neg'".to_string(),
                            ),
                            Note::new(
                                None,
                                format!("'neg' returns {} values", signature.returns.len()),
                            ),
                            Note::new(
                                None,
                                format!(
                                    "'neg' has signature {}{}{}",
                                    color!(Blue),
                                    self.types.format_signature(&signature),
                                    reset!(),
                                ),
                            ),
                        ],
                    ))
                }
            }
            Expr::Deref(expr, span) => uop!("*", expr, *span),
            Expr::Group(ops, span) => {
                let mut stack = Vec::new();
                for op in ops {
                    self.check_op(&mut stack, op)?;
                }
                match stack.len() {
                    1 => Ok(stack.pop().unwrap().1),
                    0 => Err(Error::Type(
                        *span,
                        "expression group yields no values".to_string(),
                        vec![],
                    )),
                    _ => Err(Error::Type(
                        *span,
                        "expression group yields multiple values".to_string(),
                        vec![Note::new(
                            None,
                            format!(
                                "stack is {}{}{}",
                                color!(Blue),
                                self.types.format_stack(&stack),
                                reset!()
                            ),
                        )],
                    )),
                }
            }
        }
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
                // TODO: Be less strict on generic parameters in arguments.
                //       If a function does not use all of its generic parameters
                //       in its arguments, it must be called indirectly with the
                //       "f[] call" syntax.
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
                    generic_indices.extend(self.types.generic_indices_struct(ty, name.name));
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
                self.globals.insert(name.name, ty);
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

pub struct CallGraph {
    pub active: FnInfo,
    pub graph: HashMap<FnInfo, HashSet<CallInfo>>,
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
        self.graph.entry(self.active).or_insert_with(HashSet::new);
    }

    fn insert(&mut self, name: &'static str, index: usize, bindings: Vec<GTypeId>) {
        self.graph
            .get_mut(&self.active)
            .unwrap()
            .insert((name, index, bindings));
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
        let mut binds = vec![None; types.generic_indices_signature(signature).len()];
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
        Some(binds.into_iter().map(Option::unwrap).collect())
    }
}

fn flatten_expr(expr: &Expr, ops: &mut Vec<Op>) {
    match expr {
        Expr::Int(val, span) => ops.push(Op::Int(*val, *span)),
        Expr::Float(val, span) => ops.push(Op::Float(*val, *span)),
        Expr::Bool(val, span) => ops.push(Op::Bool(*val, *span)),
        Expr::Char(val, span) => ops.push(Op::Char(*val, *span)),
        Expr::Byte(val, span) => ops.push(Op::Byte(*val, *span)),
        Expr::String(val, span) => ops.push(Op::String(val.clone(), *span)),
        Expr::Name(name) => ops.push(Op::Name(*name)),
        Expr::Add(left, right, span)
        | Expr::And(left, right, span)
        | Expr::Divide(left, right, span)
        | Expr::Equal(left, right, span)
        | Expr::GreaterEqual(left, right, span)
        | Expr::GreaterThan(left, right, span)
        | Expr::LessEqual(left, right, span)
        | Expr::LessThan(left, right, span)
        | Expr::Modulo(left, right, span)
        | Expr::Multiply(left, right, span)
        | Expr::NotEqual(left, right, span)
        | Expr::Or(left, right, span)
        | Expr::Subtract(left, right, span)
        | Expr::Xor(left, right, span)
        | Expr::LShift(left, right, span)
        | Expr::RShift(left, right, span) => {
            flatten_expr(left, ops);
            flatten_expr(right, ops);
            ops.push(Op::Name((*span).into()));
        }
        Expr::Not(expr, span) | Expr::Deref(expr, span) => {
            flatten_expr(expr, ops);
            ops.push(Op::Name((*span).into()));
        }
        Expr::Negate(expr, span) => {
            flatten_expr(expr, ops);
            let name = Name {
                name: "neg",
                span: *span,
            };
            ops.push(Op::Name(name));
        }
        Expr::Group(group, _) => {
            for op in group {
                match op {
                    Op::Expr(expr, _) => flatten_expr(expr, ops),
                    op => ops.push(op.clone()),
                }
            }
        }
    }
}

fn leak(s: String) -> &'static str {
    Box::leak(Box::new(s))
}

fn check_for_conflicts(
    types: &mut Types,
    existing_signatures: &[GSignature],
    new_signature: &GSignature,
) -> Result<(), Error> {
    let span = new_signature.kind.span().unwrap();
    for existing in existing_signatures {
        if params_overlap(types, &existing.params, &new_signature.params) {
            let previous_def = if let Some(existing_span) = existing.kind.span() {
                if existing_span.is_std {
                    let split_path: Vec<_> = existing_span.file.split('/').collect();
                    let path = split_path.join(":");
                    Note::new(None, format!("'{}' is defined in {}", span.text, path))
                } else if existing.kind.is_auto() {
                    Note::new(
                        Some(existing_span),
                        format!("'{}' was autogenerated here", span.text),
                    )
                } else {
                    Note::new(
                        Some(existing_span),
                        "previous definition is here".to_string(),
                    )
                }
            } else {
                Note::new(None, format!("'{}' is a builtin function", span.text))
            };
            let previous_signature = Note::new(
                None,
                format!(
                    "'{}' has signature {}{}{}",
                    span.text,
                    color!(Blue),
                    types.format_signature(existing),
                    reset!(),
                ),
            );
            return Err(Error::Type(
                span,
                "signature conflicts with a previous definition".to_string(),
                vec![previous_def, previous_signature],
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
    pub program: Program,
    pub call_graph: HashMap<FnInfo, HashSet<CallInfo>>,
    pub signatures: HashMap<&'static str, Vec<GSignature>>,
    pub struct_fields: HashMap<&'static str, Vec<(&'static str, GTypeId)>>,
    pub types: Types,
}

impl ProgramInfo {
    #[allow(dead_code)]
    pub fn display(&self) {
        self.program.display(&self.types);
    }
}

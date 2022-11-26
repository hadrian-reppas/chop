use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use std::mem;

use petgraph::algo::toposort;
use petgraph::graph::Graph;

use crate::ast::{
    ElsePart, Expr, Field, ImportGroup, Item, Name, Op, PType, QualifiedName, Stmt, TypeGenerics,
};
use crate::builtins::{Builtins, BUILTINS};
use crate::error::{Error, Note};
use crate::lex::Span;
use crate::program::{Program, ProgramContext, ProgramMember, ProgramOp, ProgramStruct};
use crate::types::{GSignature, GTypeId, Kind, Types};
use crate::{color, reset};

// TODO: topological sort on global uses in global inits (check
//       call graph to find dependencies)

pub fn check(
    units: &HashMap<Vec<&'static str>, Vec<Item>>,
    main_unit_prefix: &[&'static str],
) -> Result<ProgramInfo, Error> {
    let mut context = Context::new(&BUILTINS);
    context.check_units(units, main_unit_prefix)?;
    Ok(ProgramInfo {
        program: context.program_context.program,
        call_graph: context.call_graph.graph,
        signatures: context.signatures,
        struct_fields: context.struct_fields,
        types: context.types,
        main_unit_prefix: main_unit_prefix.to_vec(),
    })
}

pub struct Context {
    types: Types,
    signatures: HashMap<Vec<&'static str>, Vec<GSignature>>,
    struct_fields: HashMap<Vec<&'static str>, Vec<(&'static str, GTypeId)>>,
    let_binds: Vec<HashMap<&'static str, (usize, GTypeId)>>,
    function_qual: HashMap<&'static str, Vec<Vec<&'static str>>>,
    default_function_qual: HashMap<&'static str, Vec<Vec<&'static str>>>,
    struct_qual: HashMap<&'static str, Vec<&'static str>>,
    module_qual: HashMap<&'static str, Vec<&'static str>>,
    structs: HashMap<Vec<&'static str>, Span>,
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
                    vec![*name],
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
        let default_function_qual = builtins
            .iter()
            .map(|(name, _)| (*name, vec![vec![*name]]))
            .collect();
        Context {
            types,
            signatures,
            struct_fields: HashMap::new(),
            let_binds: Vec::new(),
            function_qual: HashMap::new(),
            default_function_qual,
            struct_qual: HashMap::new(),
            module_qual: HashMap::new(),
            structs: HashMap::new(),
            call_graph: CallGraph::new(),
            globals: HashMap::new(),
            generic_names: Vec::new(),
            program_context: ProgramContext::new(),
        }
    }

    fn check_units(
        &mut self,
        units: &HashMap<Vec<&'static str>, Vec<Item>>,
        main_unit_prefix: &[&'static str],
    ) -> Result<(), Error> {
        self.init_structs(units)?;
        for (prefix, unit) in units {
            self.set_struct_qual(unit, prefix)?;
            self.update_signatures(unit, prefix, main_unit_prefix)?;
        }
        if !self
            .signatures
            .contains_key(&make_names(main_unit_prefix, "main"))
        {
            return Err(Error::Main(
                None,
                format!("no function named 'main' in '{}.hop'", main_unit_prefix[0]),
                vec![],
            ));
        }
        self.check_for_recursive_structs(units)?;
        for (prefix, unit) in units {
            self.set_struct_qual(unit, prefix)?;
            self.set_function_qual(unit, prefix)?;
            self.check_unit(unit, prefix, main_unit_prefix)?;
        }
        Ok(())
    }

    fn update_signatures(
        &mut self,
        unit: &[Item],
        prefix: &[&'static str],
        main_unit_prefix: &[&'static str],
    ) -> Result<(), Error> {
        for item in unit {
            for (name, signature) in self.get_signatures(item, prefix, main_unit_prefix)? {
                self.insert_signature(name, signature)?;
            }
        }
        Ok(())
    }

    fn set_struct_qual(&mut self, unit: &[Item], prefix: &[&'static str]) -> Result<(), Error> {
        self.struct_qual.clear();
        self.module_qual.clear();
        let mut module_spans = HashMap::new();
        let mut struct_spans = HashMap::new();
        for item in unit {
            match item {
                Item::Import {
                    names: path,
                    group: Some(ImportGroup { names, .. }),
                    struct_span: Some(_),
                    ..
                } => {
                    let mut full_path = if path[0].name == "std" {
                        Vec::new()
                    } else {
                        prefix[..prefix.len() - 1].to_vec()
                    };
                    full_path.extend(path.iter().map(|n| n.name));
                    for name in names {
                        full_path.push(name.name);
                        match self.struct_qual.entry(name.name) {
                            Entry::Occupied(_) => {
                                let note = match struct_spans.get(name.name).unwrap() {
                                    ImportKind::Import(span) => Note::new(
                                        Some(*span),
                                        format!("'{}' was imported here", name.name),
                                    ),
                                    ImportKind::Custom(span) => Note::new(
                                        Some(*span),
                                        format!("'{}' was defined here", name.name),
                                    ),
                                };
                                return Err(Error::Import(
                                    Some(name.span),
                                    format!("struct '{}' is already in scope", name.name),
                                    vec![note],
                                ));
                            }
                            Entry::Vacant(v) => {
                                struct_spans.insert(name.name, ImportKind::Import(name.span));
                                v.insert(full_path.clone());
                            }
                        }
                        full_path.pop();
                    }
                }
                Item::Import {
                    names, group: None, ..
                } => {
                    let module = *names.last().unwrap();
                    if let Some(span) = module_spans.get(module.name) {
                        return Err(Error::Import(
                            Some(module.span),
                            format!("a module named '{}' has already been imported", module.name,),
                            vec![Note::new(
                                Some(*span),
                                format!("'{}' was imported here", module.name),
                            )],
                        ));
                    }
                    module_spans.insert(module.name, module.span);

                    let mut full_path = if names[0].name == "std" {
                        Vec::new()
                    } else {
                        prefix[..prefix.len() - 1].to_vec()
                    };
                    full_path.extend(names.iter().map(|n| n.name));
                    self.module_qual.insert(module.name, full_path);
                }
                Item::Struct { name, .. } => {
                    match struct_spans.entry(name.name) {
                        Entry::Occupied(o) => match o.get() {
                            ImportKind::Import(span) => {
                                return Err(Error::Import(
                                    Some(name.span),
                                    format!(
                                        "a different struct '{}' is already in scope",
                                        name.name
                                    ),
                                    vec![Note::new(
                                        Some(*span),
                                        format!("'{}' was imported here", name.name),
                                    )],
                                ))
                            }
                            _ => unreachable!(),
                        },
                        Entry::Vacant(v) => {
                            v.insert(ImportKind::Custom(name.span));
                        }
                    }
                    self.struct_qual
                        .insert(name.name, make_names(prefix, name.name));
                }
                _ => {}
            }
        }
        Ok(())
    }

    fn set_function_qual(&mut self, unit: &[Item], prefix: &[&'static str]) -> Result<(), Error> {
        self.function_qual = self.default_function_qual.clone();
        let mut fn_set = HashSet::new();
        macro_rules! insert {
            ($name:expr, $path:expr) => {
                if fn_set.insert($name) {
                    match self.function_qual.entry($name) {
                        Entry::Occupied(mut o) => o.get_mut().push($path),
                        Entry::Vacant(v) => {
                            v.insert(vec![$path]);
                        }
                    }
                }
            };
        }
        for item in unit {
            match item {
                Item::Function { name, .. } => insert!(name.name, make_names(prefix, name.name)),
                Item::Struct { name, fields, .. } => {
                    insert!(name.name, make_names(prefix, name.name));
                    for Field { name: fname, .. } in fields {
                        let dotdot = leak(format!("..{}", fname.name));
                        let dot = dotdot.strip_prefix('.').unwrap();
                        insert!(dotdot, make_names(prefix, dotdot));
                        insert!(dot, make_names(prefix, dot));
                    }
                }
                Item::Global { name, .. } => {
                    let leak = leak(format!("write_{}_ptr", name.name));
                    let write = leak.strip_suffix("_ptr").unwrap();
                    let ptr = leak.strip_prefix("write_").unwrap();
                    insert!(name.name, make_names(prefix, name.name));
                    insert!(write, make_names(prefix, write));
                    insert!(ptr, make_names(prefix, ptr));
                }
                _ => {}
            }
        }
        fn import_filter(item: &Item) -> Option<(&Vec<Name>, &Vec<Name>)> {
            if let Item::Import {
                names: path,
                group: Some(ImportGroup { names, .. }),
                struct_span: None,
                ..
            } = item
            {
                Some((path, names))
            } else {
                None
            }
        }
        let mut import_spans = HashMap::new();
        for (path, names) in unit.iter().filter_map(import_filter) {
            let mut full_path = if path[0].name == "std" {
                Vec::new()
            } else {
                prefix[..prefix.len() - 1].to_vec()
            };
            full_path.extend(path.iter().map(|n| n.name));
            for name in names {
                full_path.push(name.name);
                if let Some(import_sigs) = self.signatures.get(&full_path) {
                    for qual_fn in self.function_qual.get(name.name).into_iter().flatten() {
                        for existing_sig in &self.signatures[qual_fn] {
                            for new_sig in import_sigs {
                                if params_overlap(
                                    &mut self.types,
                                    &existing_sig.params,
                                    &new_sig.params,
                                ) {
                                    if let Some(import_span) = import_spans.get(qual_fn) {
                                        return Err(Error::Import(
                                            Some(name.span),
                                            format!(
                                                "imported function '{}' overlaps with a previous import",
                                                name.name
                                            ),
                                            vec![
                                                Note::new(
                                                    Some(*import_span),
                                                    format!("'{}' is imported here", name.name),
                                                ),
                                                Note::new(
                                                    None,
                                                    format!(
                                                        "previously imported signature is {}{}{}",
                                                        color!(Blue),
                                                        self.types.format_signature(existing_sig),
                                                        reset!()
                                                    ),
                                                ),
                                                Note::new(
                                                    None,
                                                    format!(
                                                        "new siganture is {}{}{}",
                                                        color!(Blue),
                                                        self.types.format_signature(new_sig),
                                                        reset!()
                                                    ),
                                                ),
                                            ],
                                        ));
                                    } else {
                                        match existing_sig.kind {
                                            Kind::Builtin => return Err(Error::Import(
                                                Some(name.span),
                                                "import conflicts with a builtin function".to_string(),
                                                vec![
                                                    Note::new(
                                                        None,
                                                        format!("builtin function '{}' has signature {}{}{}",
                                                            name.name,
                                                            color!(Blue),
                                                            self.types.format_signature(existing_sig),
                                                            reset!()
                                                        )
                                                    ),
                                                    Note::new(
                                                        None,
                                                        format!("imported function '{}' has signature {}{}{}",
                                                            name.name,
                                                            color!(Blue),
                                                            self.types.format_signature(new_sig),
                                                            reset!()
                                                        )
                                                    ),
                                                ]
                                            )),
                                            Kind::Custom(span) => return Err(Error::Import(
                                                Some(name.span),
                                                "imported function conflicts with a previously defined function".to_string(),
                                                vec![
                                                    Note::new(Some(span), format!("'{}' is defined here", name.name)),
                                                    Note::new(
                                                        None,
                                                        format!("definition of '{}' has signature {}{}{}",
                                                            name.name,
                                                            color!(Blue),
                                                            self.types.format_signature(existing_sig),
                                                            reset!()
                                                        )
                                                    ),
                                                    Note::new(
                                                        None,
                                                        format!("imported function '{}' has signature {}{}{}",
                                                            name.name,
                                                            color!(Blue),
                                                            self.types.format_signature(new_sig),
                                                            reset!()
                                                        )
                                                    ),
                                                ]
                                            )),
                                            Kind::Constructor(span)
                                            | Kind::Member(span)
                                            | Kind::Global(span)
                                            | Kind::GlobalWrite(span)
                                            | Kind::GlobalPtr(span) => return Err(Error::Import(
                                                Some(name.span),
                                                "imported function conflicts with an autogenerated function".to_string(),
                                                vec![
                                                    Note::new(Some(span), format!("'{}' is generated here", name.name)),
                                                    Note::new(
                                                        None,
                                                        format!("generated function '{}' has signature {}{}{}",
                                                            name.name,
                                                            color!(Blue),
                                                            self.types.format_signature(existing_sig),
                                                            reset!()
                                                        )
                                                    ),
                                                    Note::new(
                                                        None,
                                                        format!("imported function '{}' has signature {}{}{}",
                                                            name.name,
                                                            color!(Blue),
                                                            self.types.format_signature(new_sig),
                                                            reset!()
                                                        )
                                                    ),
                                                ]
                                            )),
                                        }
                                    }
                                }
                            }
                        }
                    }
                    match self.function_qual.entry(name.name) {
                        Entry::Occupied(mut o) => o.get_mut().push(full_path.clone()),
                        Entry::Vacant(v) => {
                            v.insert(vec![full_path.clone()]);
                        }
                    }
                    import_spans.insert(full_path.clone(), name.span);
                } else {
                    return Err(Error::Import(
                        Some(name.span),
                        format!(
                            "no function '{}' in module '{}'",
                            name.name,
                            path.last().unwrap().name
                        ),
                        vec![],
                    ));
                }
                full_path.pop();
            }
        }
        Ok(())
    }

    fn check_unit(
        &mut self,
        unit: &[Item],
        prefix: &[&'static str],
        main_unit_prefix: &[&'static str],
    ) -> Result<(), Error> {
        for item in unit {
            self.check_item(item, prefix, main_unit_prefix)?;
        }
        Ok(())
    }

    fn check_for_recursive_structs(
        &mut self,
        units: &HashMap<Vec<&str>, Vec<Item>>,
    ) -> Result<(), Error> {
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
            let name_id = get_id!(name);
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
                        let mut structs = vec![*graph.node_weight(id).unwrap()];
                        while let Some(prev_id) = prev.get(&id) {
                            id = *prev_id;
                            structs.insert(0, *graph.node_weight(id).unwrap());
                        }
                        return Err(self.make_recursive_struct_error(units, &structs));
                    }
                    prev.insert(neighbor_id, id);
                    stack.push(neighbor_id);
                }
            }
            unreachable!();
        }
        Ok(())
    }

    fn make_recursive_struct_error(
        &self,
        units: &HashMap<Vec<&str>, Vec<Item>>,
        structs: &[&Vec<&str>],
    ) -> Error {
        let struct_set: HashSet<_> = structs.iter().copied().collect();
        let mut span = None;
        'outer: for (prefix, unit) in units {
            let mut qual_struct = prefix.clone();
            for item in unit {
                if let Item::Struct { name, .. } = item {
                    qual_struct.push(name.name);
                    if struct_set.contains(&qual_struct) {
                        span = Some(name.span);
                        break 'outer;
                    }
                }
            }
        }
        let span = span.unwrap();
        let msg = match structs.len() {
            1 => format!("struct '{}' is defined recursively", structs[0].join("::")),
            2 => format!(
                "structs '{}' and '{}' are defined recursively",
                structs[0].join("::"),
                structs[1].join("::")
            ),
            _ => {
                let mut msg = "structs ".to_string();
                #[allow(clippy::needless_range_loop)]
                for i in 0..(structs.len() - 1) {
                    if i > 0 {
                        msg.push_str(", '");
                    } else {
                        msg.push('\'');
                    }
                    msg.push_str(&structs[i].join("::"));
                    msg.push('\'');
                }
                msg.push_str(" and '");
                msg.push_str(&structs.last().unwrap().join("::"));
                msg.push_str("' are defined recursively");
                msg
            }
        };
        Error::Type(span, msg, vec![])
    }

    fn init_structs(&mut self, units: &HashMap<Vec<&'static str>, Vec<Item>>) -> Result<(), Error> {
        for (prefix, unit) in units {
            let mut struct_spans = HashMap::new();
            for item in unit {
                if let Item::Struct { name, generics, .. } = item {
                    if let Some(span) = struct_spans.get(name.name) {
                        return Err(Error::Type(
                            name.span,
                            format!("struct '{}' is already defined", name.name),
                            vec![Note::new(
                                Some(*span),
                                "previous definition is here".to_string(),
                            )],
                        ));
                    }
                    let count = match generics {
                        Some(generics) => generics.names.len(),
                        None => 0,
                    };
                    self.types
                        .generic_counts
                        .insert(make_names(prefix, name.name), count);
                    self.structs
                        .insert(make_names(prefix, name.name), name.span);
                    struct_spans.insert(name.name, name.span);
                }
            }
        }
        Ok(())
    }

    fn check_item(
        &mut self,
        item: &Item,
        prefix: &[&'static str],
        main_unit_prefix: &[&'static str],
    ) -> Result<(), Error> {
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
                    .map(|ty| {
                        self.types.convert(
                            ty,
                            &self.generic_names,
                            &self.struct_qual,
                            &self.module_qual,
                        )
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                let returns = returns
                    .iter()
                    .map(|ty| {
                        self.types.convert(
                            ty,
                            &self.generic_names,
                            &self.struct_qual,
                            &self.module_qual,
                        )
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                let mut qual_name = prefix.to_vec();
                qual_name.push(name.name);
                let index = self
                    .signatures
                    .get(&qual_name)
                    .unwrap()
                    .iter()
                    .position(|sig| sig.params == params)
                    .unwrap();
                self.call_graph.set_active(qual_name, index);
                let mut stack: Vec<_> = params.iter().copied().enumerate().collect();
                self.program_context.begin_fn(&params);
                for stmt in body {
                    self.check_stmt(&mut stack, stmt)?;
                }
                let stack_types: Vec<_> = stack.iter().map(|(_, ty)| *ty).collect();
                if stack_types == returns {
                    self.program_context.end_fn(
                        &stack,
                        make_names(prefix, name.name),
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
                                    reset!()
                                ),
                            ),
                            Note::new(
                                None,
                                format!(
                                    "found {}{}{}",
                                    color!(Blue),
                                    self.types.format_types(&stack_types),
                                    reset!()
                                ),
                            ),
                        ],
                    ))
                }
            }
            Item::Struct { name, generics, .. } => {
                let mut members = Vec::new();
                let mut qual_name = prefix.to_vec();
                qual_name.push(name.name);
                for (name, type_id) in self.struct_fields.get(&qual_name).unwrap() {
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
                let ty = self.types.convert(
                    ty,
                    &self.generic_names,
                    &self.struct_qual,
                    &self.module_qual,
                )?;
                let substituted = self.types.substitute_concrete(ty, &[]);
                self.program_context.begin_global();
                if let Some(definition) = definition {
                    let main_qual = make_names(main_unit_prefix, "main");
                    self.call_graph.set_active(main_qual, 0);
                    let mut stack = Vec::new();
                    for op in &definition.group {
                        self.check_op(&mut stack, op)?;
                    }
                    if stack.len() == 1 && stack[0].1 == ty {
                        self.program_context.end_global_init(
                            make_names(prefix, name.name),
                            substituted,
                            stack[0].0,
                        );
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
                                        reset!()
                                    ),
                                ),
                                Note::new(
                                    None,
                                    format!(
                                        "stack is {}{}{}",
                                        color!(Blue),
                                        self.types.format_stack(&stack),
                                        reset!()
                                    ),
                                ),
                            ],
                        ))
                    }
                } else {
                    self.program_context
                        .end_global(make_names(prefix, name.name), substituted);
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
                        reset!()
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
                lbrace_span.location()
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
                                    rbrace_span.location()
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
                                    rbrace_span.location()
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
                                else_part.rbrace_span.location()
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
                                    rbrace_span.location()
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
                                    rbrace_span.location()
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
                        reset!()
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
                            reset!()
                        ),
                    ),
                    Note::new(
                        None,
                        format!(
                            "after test, stack is {}{}{}",
                            color!(Blue),
                            self.types.format_stack(stack),
                            reset!()
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
                            reset!()
                        ),
                    ),
                    Note::new(
                        None,
                        format!(
                            "after while block, stack is {}{}{}",
                            color!(Blue),
                            self.types.format_stack(stack),
                            reset!()
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
                        reset!()
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
                            reset!()
                        ),
                    ),
                    Note::new(
                        None,
                        format!(
                            "after bound, stack is {}{}{}",
                            color!(Blue),
                            self.types.format_stack(stack),
                            reset!()
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
                        reset!()
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
                            reset!()
                        ),
                    ),
                    Note::new(
                        None,
                        format!(
                            "after bound, stack is {}{}{}",
                            color!(Blue),
                            self.types.format_stack(stack),
                            reset!()
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
                            reset!()
                        ),
                    ),
                    Note::new(
                        None,
                        format!(
                            "after for block, stack is {}{}{}",
                            color!(Blue),
                            self.types.format_stack(stack),
                            reset!()
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
                    stack.len()
                ),
                vec![Note::new(
                    None,
                    format!(
                        "stack is {}{}{}",
                        color!(Blue),
                        self.types.format_stack(stack),
                        reset!()
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
                let ty = self.types.convert(
                    ptype,
                    &self.generic_names,
                    &self.struct_qual,
                    &self.module_qual,
                )?;
                let int_id = self.types.int();
                let var = self.program_context.alloc(int_id);
                stack.push((var, int_id));
                self.program_context.push_op(ProgramOp::SizeOf(var, ty));
                Ok(())
            }
            Op::Alloc(ptype, _, _, _) => {
                let ty = self.types.convert(
                    ptype,
                    &self.generic_names,
                    &self.struct_qual,
                    &self.module_qual,
                )?;
                let ty_ptr = self.types.ref_n(ty, 1);
                let var = self.program_context.alloc(ty_ptr);
                stack.push((var, ty_ptr));
                self.program_context.push_op(ProgramOp::Alloc(var, ty));
                Ok(())
            }
            Op::Zalloc(ptype, _, _, _) => {
                let ty = self.types.convert(
                    ptype,
                    &self.generic_names,
                    &self.struct_qual,
                    &self.module_qual,
                )?;
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
                                reset!()
                            ),
                        )],
                    ));
                }
                let len_var = stack.pop().unwrap().0;
                let ty = self.types.convert(
                    ptype,
                    &self.generic_names,
                    &self.struct_qual,
                    &self.module_qual,
                )?;
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
                                reset!()
                            ),
                        )],
                    ));
                }
                let len_var = stack.pop().unwrap().0;
                let ty = self.types.convert(
                    ptype,
                    &self.generic_names,
                    &self.struct_qual,
                    &self.module_qual,
                )?;
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
                                reset!()
                            ),
                        )],
                    ));
                }
                let arg_var = stack.pop().unwrap().0;
                let ty = self.types.convert(
                    ptype,
                    &self.generic_names,
                    &self.struct_qual,
                    &self.module_qual,
                )?;
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
                                reset!()
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
            Op::Assert(span) => {
                if stack.is_empty() || (stack.last().unwrap().1 != self.types.bool()) {
                    return Err(Error::Type(
                        *span,
                        "'assert' expects a 'bool'".to_string(),
                        vec![Note::new(
                            None,
                            format!(
                                "stack is {}{}{}",
                                color!(Blue),
                                self.types.format_stack(stack),
                                reset!()
                            ),
                        )],
                    ));
                }
                let (var, _) = stack.pop().unwrap();
                self.program_context.push_op(ProgramOp::Assert(var, *span));
                Ok(())
            }
            Op::Abort(span, types) => {
                for ty in types {
                    let ty = self.types.convert(
                        ty,
                        &self.generic_names,
                        &self.struct_qual,
                        &self.module_qual,
                    )?;
                    let var = self.program_context.alloc(ty);
                    stack.push((var, ty));
                }
                self.program_context.push_op(ProgramOp::Abort(*span));
                Ok(())
            }
        }
    }

    fn check_name(
        &mut self,
        stack: &mut Vec<(usize, GTypeId)>,
        qname: &QualifiedName,
    ) -> Result<(), Error> {
        if qname.is_just("DEBUG_STACK") {
            println!(
                "{}DEBUG_STACK{} {}{}{} ({})",
                color!(Green),
                reset!(),
                color!(Blue),
                self.types.format_stack(stack),
                reset!(),
                qname.span().location()
            );
            return Ok(());
        }

        if let QualifiedName::Straight(name) = qname {
            if let Some((var, ty)) = self.get_let_bind(name.name) {
                stack.push((var, ty));
                return Ok(());
            }
        }

        let NameInfo {
            qualified,
            signature,
            binds,
            index,
        } = self.get_qsbi(stack, qname)?;
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
        if qname.is_just("@") {
            self.program_context.push_op(ProgramOp::Ref(
                return_vars[1],
                param_vars[0],
                stack[stack.len() - 2].1,
            ));
        } else {
            self.program_context.push_op(ProgramOp::Call(
                qualified,
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

    fn get_qsbi(
        &mut self,
        stack: &[(usize, GTypeId)],
        name: &QualifiedName,
    ) -> Result<NameInfo, Error> {
        match name {
            QualifiedName::Qualified(module, name) => {
                if let Some(qual_module) = self.module_qual.get(module.name) {
                    let qualified = make_names(qual_module, name.name);
                    if let Some(signatures) = self.signatures.get(&qualified) {
                        for (index, signature) in self.signatures[&qualified].iter().enumerate() {
                            if let Some(binds) = get_binds(&mut self.types, stack, signature) {
                                self.call_graph
                                    .insert(qualified.clone(), index, binds.clone());
                                return Ok(NameInfo {
                                    qualified,
                                    signature: signature.clone(),
                                    binds,
                                    index,
                                });
                            }
                        }
                        Err(Error::Type(
                            name.span,
                            format!("no variant of '{}' matches the stack", name.name),
                            Some(Note::new(
                                None,
                                format!(
                                    "stack is {}{}{}",
                                    color!(Blue),
                                    self.types.format_stack(stack),
                                    reset!()
                                ),
                            ))
                            .into_iter()
                            .chain(signatures.iter().map(|s| {
                                Note::new(
                                    None,
                                    format!(
                                        "'{}' has signature {}{}{}",
                                        name.name,
                                        color!(Blue),
                                        self.types.format_signature(s),
                                        reset!()
                                    ),
                                )
                            }))
                            .collect(),
                        ))
                    } else {
                        Err(Error::Type(
                            name.span,
                            format!("no function '{}' in module '{}'", name.name, module.name),
                            vec![],
                        ))
                    }
                } else {
                    Err(Error::Type(
                        module.span,
                        format!("no module '{}' in scope", module.name),
                        vec![],
                    ))
                }
            }
            QualifiedName::Straight(name) => {
                if let Some(qual_fns) = self.function_qual.get(name.name) {
                    for qual_fn in qual_fns {
                        for (index, signature) in self.signatures[qual_fn].iter().enumerate() {
                            if let Some(binds) = get_binds(&mut self.types, stack, signature) {
                                self.call_graph
                                    .insert(qual_fn.clone(), index, binds.clone());
                                return Ok(NameInfo {
                                    qualified: qual_fn.clone(),
                                    signature: signature.clone(),
                                    binds,
                                    index,
                                });
                            }
                        }
                    }
                    Err(Error::Type(
                        name.span,
                        format!("no variant of '{}' matches the stack", name.name),
                        Some(Note::new(
                            None,
                            format!(
                                "stack is {}{}{}",
                                color!(Blue),
                                self.types.format_stack(stack),
                                reset!()
                            ),
                        ))
                        .into_iter()
                        .chain(qual_fns.iter().flat_map(|q| &self.signatures[q]).map(|s| {
                            Note::new(
                                None,
                                format!(
                                    "'{}' has signature {}{}{}",
                                    name.name,
                                    color!(Blue),
                                    self.types.format_signature(s),
                                    reset!()
                                ),
                            )
                        }))
                        .collect(),
                    ))
                } else {
                    Err(Error::Type(
                        name.span,
                        format!("no function '{}' in scope", name.name),
                        vec![],
                    ))
                }
            }
        }
    }

    fn check_expr(&mut self, expr: &Expr) -> Result<GTypeId, Error> {
        macro_rules! binop {
            ($op:expr, $left:ident, $right:ident, $span:expr) => {{
                let stack = vec![(0, self.check_expr($left)?), (0, self.check_expr($right)?)];
                let NameInfo {
                    signature, binds, ..
                } = self.get_qsbi(&stack, &$span.into())?;
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
                                    reset!()
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
                let NameInfo {
                    signature, binds, ..
                } = self.get_qsbi(&stack, &$span.into())?;
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
                                    reset!()
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
            Expr::Name(qname) => {
                if let QualifiedName::Straight(name) = qname && let Some((_, ty)) = self.get_let_bind(name.name) {
                    Ok(ty)
                } else {
                    let NameInfo { signature, binds, .. } = self.get_qsbi(&[], qname)?;
                    if signature.returns.len() == 1 {
                        Ok(self.types.substitute(signature.returns[0], &binds))
                    } else {
                        Err(Error::Type(
                            qname.span(),
                            "expression functions must return a single value".to_string(),
                            vec![
                                Note::new(
                                    None,
                                    format!("'{}' returns {} values", qname.name().name, signature.returns.len()),
                                ),
                                Note::new(
                                    None,
                                    format!(
                                        "'{}' has signature {}{}{}",
                                        qname.name().name,
                                        color!(Blue),
                                        self.types.format_signature(&signature),
                                        reset!()
                                    ),
                                ),
                            ],
                        ))
                    }
                }
            },
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
                let qneg = QualifiedName::Straight(name);
                let (signature, binds) = match self.get_qsbi(&stack, &qneg) {
                    Ok(NameInfo {
                        signature, binds, ..
                    }) => (signature, binds),
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
                                    reset!()
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

    fn get_signatures(
        &mut self,
        item: &Item,
        prefix: &[&'static str],
        main_unit_prefix: &[&'static str],
    ) -> Result<Vec<(Vec<&'static str>, GSignature)>, Error> {
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
                    .map(|ty| {
                        self.types
                            .convert(ty, generic_names, &self.struct_qual, &self.module_qual)
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                let returns = returns
                    .iter()
                    .map(|ty| {
                        self.types
                            .convert(ty, generic_names, &self.struct_qual, &self.module_qual)
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                if name.name == "main" {
                    if prefix != main_unit_prefix {
                        return Err(Error::Main(
                            Some(name.span),
                            "the 'main' function must be in the main file".to_string(),
                            vec![Note::new(
                                None,
                                format!("main file is '{}.hop'", main_unit_prefix[0]),
                            )],
                        ));
                    } else if let Some(sigs) =
                        self.signatures.get(&make_names(main_unit_prefix, "main"))
                    {
                        return Err(Error::Main(
                            Some(name.span),
                            "multiple functions named 'main'".to_string(),
                            vec![Note::new(
                                sigs[0].kind.span(),
                                "'main' defined here".to_string(),
                            )],
                        ));
                    } else if !params.is_empty()
                        && params != [self.types.int(), self.types.byte_ptr_ptr()]
                    {
                        return Err(Error::Main(
                            Some(name.span),
                            format!(
                                "'main' must have parameters {}[]{} or {}[int, **byte]{}",
                                color!(Blue),
                                reset!(),
                                color!(Blue),
                                reset!()
                            ),
                            vec![Note::new(
                                None,
                                format!(
                                    "'main' has signature {}{} -> {}{}",
                                    color!(Blue),
                                    self.types.format_types(&params),
                                    self.types.format_types(&returns),
                                    reset!()
                                ),
                            )],
                        ));
                    } else if !returns.is_empty() && returns != [self.types.int()] {
                        return Err(Error::Main(
                            Some(name.span),
                            format!(
                                "'main' must return {}[]{} or {}[int]{}",
                                color!(Blue),
                                reset!(),
                                color!(Blue),
                                reset!()
                            ),
                            vec![Note::new(
                                None,
                                format!(
                                    "'main' has signature {}{} -> {}{}",
                                    color!(Blue),
                                    self.types.format_types(&params),
                                    self.types.format_types(&returns),
                                    reset!()
                                ),
                            )],
                        ));
                    }
                }

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
                    make_names(prefix, name.name),
                    GSignature::new(params, returns, Kind::Custom(name.span)),
                )])
            }
            Item::Struct {
                name,
                generics,
                fields,
                ..
            } => {
                if name.name == "main" {
                    return Err(Error::Main(
                        Some(name.span),
                        "name 'main' is reserved".to_string(),
                        vec![],
                    ));
                }
                let empty = Vec::new();
                let generic_names = match generics {
                    Some(gen) => &gen.names,
                    None => &empty,
                };
                let ptype = PType {
                    stars: None,
                    name: name.span.into(),
                    generics: generics.as_ref().map(|generics| TypeGenerics {
                        types: generics
                            .names
                            .iter()
                            .map(|n| PType {
                                stars: None,
                                name: n.span.into(),
                                generics: None,
                            })
                            .collect(),
                        lbrack_span: Span::empty(),
                        rbrack_span: Span::empty(),
                    }),
                };
                let struct_ty = self.types.convert(
                    &ptype,
                    generic_names,
                    &self.struct_qual,
                    &self.module_qual,
                )?;
                let struct_ty_ptr = self.types.ref_n(struct_ty, 1);
                let mut constructor_params = Vec::new();
                let mut signatures = Vec::new();
                let mut struct_fields = Vec::new();
                let mut generic_indices = HashSet::new();
                let qname = make_names(prefix, name.name);
                for Field {
                    name: fname, ty, ..
                } in fields
                {
                    let ty = self.types.convert(
                        ty,
                        generic_names,
                        &self.struct_qual,
                        &self.module_qual,
                    )?;
                    constructor_params.push(ty);
                    struct_fields.push((fname.name, ty));
                    generic_indices.extend(self.types.generic_indices_struct(ty, &qname));
                    let ty_ptr = self.types.ref_n(ty, 1);
                    let dotdot = leak(format!("..{}", fname.name));
                    let dot = dotdot.strip_prefix('.').unwrap();
                    signatures.push((
                        make_names(prefix, dot),
                        GSignature::new(vec![struct_ty], vec![ty], Kind::Member(name.span)),
                    ));
                    signatures.push((
                        make_names(prefix, dotdot),
                        GSignature::new(
                            vec![struct_ty],
                            vec![struct_ty, ty],
                            Kind::Member(name.span),
                        ),
                    ));
                    signatures.push((
                        make_names(prefix, dot),
                        GSignature::new(vec![struct_ty_ptr], vec![ty_ptr], Kind::Member(name.span)),
                    ));
                    signatures.push((
                        make_names(prefix, dotdot),
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
                    qname.clone(),
                    GSignature::new(
                        constructor_params,
                        vec![struct_ty],
                        Kind::Constructor(name.span),
                    ),
                ));
                self.struct_fields.insert(qname, struct_fields);
                Ok(signatures)
            }
            Item::Global { name, ty, .. } => {
                if name.name == "main" {
                    return Err(Error::Main(
                        Some(name.span),
                        "name 'main' is reserved".to_string(),
                        vec![],
                    ));
                }
                let ty = self
                    .types
                    .convert(ty, &[], &self.struct_qual, &self.module_qual)?;
                self.globals.insert(name.name, ty);
                let write_name_ptr = leak(format!("write_{}_ptr", name.name));
                let write_name = write_name_ptr.strip_suffix("_ptr").unwrap();
                let name_ptr = write_name_ptr.strip_prefix("write_").unwrap();
                Ok(vec![
                    (
                        make_names(prefix, name.name),
                        GSignature::new(vec![], vec![ty], Kind::Global(name.span)),
                    ),
                    (
                        make_names(prefix, write_name),
                        GSignature::new(vec![ty], vec![], Kind::GlobalWrite(name.span)),
                    ),
                    (
                        make_names(prefix, name_ptr),
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

    fn insert_signature(
        &mut self,
        name: Vec<&'static str>,
        signature: GSignature,
    ) -> Result<(), Error> {
        let last = *name.last().unwrap();
        match self.signatures.entry(name) {
            Entry::Occupied(mut o) => {
                check_for_conflicts(&mut self.types, o.get(), &signature, last)?;
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

enum ImportKind {
    Custom(Span),
    Import(Span),
}

struct NameInfo {
    qualified: Vec<&'static str>,
    signature: GSignature,
    binds: Vec<GTypeId>,
    index: usize,
}

fn make_names(prefix: &[&'static str], name: &'static str) -> Vec<&'static str> {
    let mut names = prefix.to_vec();
    names.push(name);
    names
}

pub type FnInfo = (Vec<&'static str>, usize);
pub type CallInfo = (Vec<&'static str>, usize, Vec<GTypeId>);

pub struct CallGraph {
    pub active: FnInfo,
    pub graph: HashMap<FnInfo, HashSet<CallInfo>>,
}

impl CallGraph {
    fn new() -> CallGraph {
        CallGraph {
            active: (Vec::new(), 0),
            graph: HashMap::new(),
        }
    }

    fn set_active(&mut self, name: Vec<&'static str>, index: usize) {
        self.active = (name, index);
        self.graph
            .entry(self.active.clone())
            .or_insert_with(HashSet::new);
    }

    fn insert(&mut self, name: Vec<&'static str>, index: usize, bindings: Vec<GTypeId>) {
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
        Expr::Name(qname) => ops.push(Op::Name(*qname)),
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
            let qname = QualifiedName::Straight(name);
            ops.push(Op::Name(qname));
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
    name: &str,
) -> Result<(), Error> {
    let span = new_signature.kind.span().unwrap();
    for existing in existing_signatures {
        if params_overlap(types, &existing.params, &new_signature.params) {
            let previous_def = if let Some(existing_span) = existing.kind.span() {
                if existing.kind.is_auto() {
                    Note::new(
                        Some(existing_span),
                        format!("'{}' was autogenerated here", name),
                    )
                } else {
                    Note::new(
                        Some(existing_span),
                        "previous definition is here".to_string(),
                    )
                }
            } else {
                Note::new(None, format!("'{}' is a builtin function", name))
            };
            let previous_signature = Note::new(
                None,
                format!(
                    "'{name}' has signature {}{}{}",
                    color!(Blue),
                    types.format_signature(existing),
                    reset!()
                ),
            );
            if new_signature.kind.is_auto() {
                return Err(Error::Type(
                    span,
                    format!("autogenerated function '{name}' conflicts with a previous definition"),
                    vec![previous_def, previous_signature],
                ));
            } else {
                return Err(Error::Type(
                    span,
                    "signature conflicts with a previous definition".to_string(),
                    vec![previous_def, previous_signature],
                ));
            }
        }
    }
    Ok(())
}

// TODO: is this correct?
fn params_overlap(types: &mut Types, params_a: &[GTypeId], params_b: &[GTypeId]) -> bool {
    can_bind(types, params_a, params_b) || can_bind(types, params_b, params_a)
}

fn can_bind(types: &mut Types, params_a: &[GTypeId], params_b: &[GTypeId]) -> bool {
    if params_a.len() < params_b.len() {
        false
    } else {
        let mut binds = HashMap::new();
        for (a, b) in params_a.iter().rev().zip(params_b.iter().rev()) {
            match types.bind(*b, *a) {
                Ok(map) => {
                    for (index, ty) in map {
                        if let Some(bound) = binds.get(&index) {
                            if bound != &ty {
                                return false;
                            }
                        } else {
                            binds.insert(index, ty);
                        }
                    }
                }
                Err(_) => return false,
            }
        }
        true
    }
}

pub struct ProgramInfo {
    pub program: Program,
    pub call_graph: HashMap<FnInfo, HashSet<CallInfo>>,
    pub signatures: HashMap<Vec<&'static str>, Vec<GSignature>>,
    pub struct_fields: HashMap<Vec<&'static str>, Vec<(&'static str, GTypeId)>>,
    pub types: Types,
    pub main_unit_prefix: Vec<&'static str>,
}

impl ProgramInfo {
    #[allow(dead_code)]
    pub fn display(&self) {
        self.program.display(&self.types);
    }
}

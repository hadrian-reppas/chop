use std::collections::{hash_map, HashMap, HashSet};

use bimap::BiMap;
use petgraph::algo::toposort;
use petgraph::Graph;

use crate::ast::{Name, PType, QualifiedName};
use crate::codegen;
use crate::error::Error;
use crate::lex::Span;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(usize);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct GTypeId(usize);

#[derive(Clone, Copy)]
pub enum Kind {
    Builtin,
    Custom(Span),
    Constructor(Span),
    Member(Span),
    Global(Span),
    GlobalWrite(Span),
    GlobalPtr(Span),
}

impl Kind {
    pub fn span(self) -> Option<Span> {
        match self {
            Kind::Builtin => None,
            Kind::Custom(span)
            | Kind::Constructor(span)
            | Kind::Member(span)
            | Kind::Global(span)
            | Kind::GlobalWrite(span)
            | Kind::GlobalPtr(span) => Some(span),
        }
    }

    pub fn is_auto(self) -> bool {
        !matches!(self, Kind::Builtin | Kind::Custom(_))
    }
}

#[derive(Clone)]
pub struct GSignature {
    pub params: Vec<GTypeId>,
    pub returns: Vec<GTypeId>,
    pub kind: Kind,
    pub is_special: bool,
    pub generic_count: usize,
}

impl GSignature {
    pub fn new(
        params: Vec<GTypeId>,
        returns: Vec<GTypeId>,
        kind: Kind,
        is_special: bool,
        generic_count: usize,
    ) -> GSignature {
        GSignature {
            params,
            returns,
            kind,
            is_special,
            generic_count,
        }
    }
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum GType {
    Int(usize),
    Float(usize),
    Byte(usize),
    Bool(usize),
    Custom(usize, Vec<&'static str>, Vec<GTypeId>),
    Generic(usize, usize),
    FnPtr(usize, Vec<GTypeId>, Vec<GTypeId>),
}

impl GType {
    pub fn ref_n(mut self, n: usize) -> GType {
        match &mut self {
            GType::Int(depth)
            | GType::Float(depth)
            | GType::Byte(depth)
            | GType::Bool(depth)
            | GType::Custom(depth, _, _)
            | GType::Generic(depth, _)
            | GType::FnPtr(depth, _, _) => *depth += n,
        }
        self
    }

    pub fn deref_n(mut self, n: usize) -> Option<GType> {
        match &mut self {
            GType::Int(depth)
            | GType::Float(depth)
            | GType::Byte(depth)
            | GType::Bool(depth)
            | GType::Custom(depth, _, _)
            | GType::Generic(depth, _)
            | GType::FnPtr(depth, _, _) => *depth = depth.checked_sub(n)?,
        }
        Some(self)
    }

    pub fn depth(&self) -> usize {
        match self {
            GType::Int(depth)
            | GType::Float(depth)
            | GType::Byte(depth)
            | GType::Bool(depth)
            | GType::Custom(depth, _, _)
            | GType::Generic(depth, _)
            | GType::FnPtr(depth, _, _) => *depth,
        }
    }
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum Type {
    Int(usize),
    Float(usize),
    Byte(usize),
    Bool(usize),
    Custom(usize, Vec<&'static str>, Vec<TypeId>),
    FnPtr(usize, Vec<TypeId>, Vec<TypeId>),
}

impl Type {
    pub fn ref_n(mut self, n: usize) -> Type {
        match &mut self {
            Type::Int(depth)
            | Type::Float(depth)
            | Type::Byte(depth)
            | Type::Bool(depth)
            | Type::Custom(depth, _, _)
            | Type::FnPtr(depth, _, _) => *depth += n,
        }
        self
    }

    fn depth(&self) -> usize {
        match self {
            Type::Int(depth)
            | Type::Float(depth)
            | Type::Byte(depth)
            | Type::Bool(depth)
            | Type::Custom(depth, _, _)
            | Type::FnPtr(depth, _, _) => *depth,
        }
    }

    fn deref_n(mut self, n: usize) -> Option<Type> {
        match &mut self {
            Type::Int(depth)
            | Type::Float(depth)
            | Type::Byte(depth)
            | Type::Bool(depth)
            | Type::Custom(depth, _, _)
            | Type::FnPtr(depth, _, _) => *depth = depth.checked_sub(n)?,
        }
        Some(self)
    }
}

pub struct Types {
    types: BiMap<Type, TypeId>,
    gtypes: BiMap<GType, GTypeId>,
    pub generic_counts: HashMap<Vec<&'static str>, usize>,
    pub custom_map: HashMap<Vec<&'static str>, HashMap<Vec<TypeId>, usize>>,
    counter: usize,
}

impl Types {
    pub fn new() -> Types {
        Types {
            types: BiMap::new(),
            gtypes: BiMap::new(),
            generic_counts: HashMap::from([
                (vec!["int"], 0),
                (vec!["float"], 0),
                (vec!["byte"], 0),
                (vec!["bool"], 0),
            ]),
            custom_map: HashMap::new(),
            counter: 0,
        }
    }

    pub fn get_or_insert(&mut self, ty: GType) -> GTypeId {
        if let Some(id) = self.gtypes.get_by_left(&ty) {
            *id
        } else {
            self.gtypes.insert(ty, GTypeId(self.counter));
            self.counter += 1;
            GTypeId(self.counter - 1)
        }
    }

    pub fn get_or_insert_concrete(&mut self, ty: Type) -> TypeId {
        if let Some(id) = self.types.get_by_left(&ty) {
            *id
        } else {
            if let Type::Custom(_, name, generics) = &ty {
                match self.custom_map.entry(name.clone()) {
                    hash_map::Entry::Occupied(mut o) => {
                        if !o.get().contains_key(generics) {
                            o.get_mut().insert(generics.clone(), self.counter);
                        }
                    }
                    hash_map::Entry::Vacant(v) => {
                        v.insert(HashMap::from([(generics.clone(), self.counter)]));
                    }
                }
            }
            self.types.insert(ty, TypeId(self.counter));
            self.counter += 1;
            TypeId(self.counter - 1)
        }
    }

    pub fn deref_n_concrete(&mut self, id: TypeId, n: usize) -> TypeId {
        let ty = self
            .types
            .get_by_right(&id)
            .unwrap()
            .clone()
            .deref_n(n)
            .unwrap();
        self.get_or_insert_concrete(ty)
    }

    pub fn is_fn_ptr(&self, id: GTypeId) -> bool {
        match self.gtypes.get_by_right(&id).unwrap() {
            GType::FnPtr(depth, _, _) => *depth == 0,
            _ => false,
        }
    }

    pub fn fn_ptr_types(&self, id: GTypeId) -> (Vec<GTypeId>, Vec<GTypeId>) {
        match self.gtypes.get_by_right(&id).unwrap() {
            GType::FnPtr(_, params, returns) => (params.clone(), returns.clone()),
            _ => unreachable!(),
        }
    }

    pub fn convert(
        &mut self,
        ty: &PType,
        generics: &[Name],
        struct_qual: &HashMap<&'static str, Vec<&'static str>>,
        module_qual: &HashMap<&'static str, Vec<&'static str>>,
    ) -> Result<GTypeId, Error> {
        match ty {
            PType::Value(stars, qname, generic_params) => self.convert_value(
                *stars,
                *qname,
                generic_params,
                generics,
                struct_qual,
                module_qual,
            ),
            PType::FnPtr(stars, params, returns) => {
                self.convert_fn_ptr(*stars, params, returns, generics, struct_qual, module_qual)
            }
        }
    }

    fn convert_value(
        &mut self,
        stars: Option<Name>,
        qname: QualifiedName,
        generic_params: &[PType],
        generics: &[Name],
        struct_qual: &HashMap<&'static str, Vec<&'static str>>,
        module_qual: &HashMap<&'static str, Vec<&'static str>>,
    ) -> Result<GTypeId, Error> {
        let span = qname.span();
        let name = span.text;
        let (generic_params, qualified) =
            if let QualifiedName::Straight(name) = qname
                && generics.iter().any(|g| g.name == name.name)
            {
                if !generic_params.is_empty() {
                    return Err(Error::Type(
                        name.span,
                        "generics cannot have generic parameters".to_string(),
                        vec![],
                    ));
                }
                (Vec::new(), Vec::new())
            } else if qname.is_just("int")
                || qname.is_just("float")
                || qname.is_just("byte")
                || qname.is_just("bool")
            {
                (Vec::new(), Vec::new())
            } else {
                let (generic_count, qual_struct) = match qname {
                    QualifiedName::Straight(name) => {
                        if let Some(qual_struct) = struct_qual.get(name.name) {
                            (
                                *self.generic_counts.get(qual_struct).unwrap(),
                                qual_struct.clone(),
                            )
                        } else {
                            return Err(Error::Type(
                                name.span,
                                format!("no type '{}' in scope", name.name),
                                vec![],
                            ));
                        }
                    }
                    QualifiedName::Qualified(module, name) => {
                        if let Some(qual_module) = module_qual.get(module.name) {
                            let mut qual_struct = qual_module.clone();
                            qual_struct.push(name.name);
                            if let Some(generic_count) = self.generic_counts.get(&qual_struct) {
                                (*generic_count, qual_struct)
                            } else {
                                return Err(Error::Type(
                                    name.span,
                                    format!("no type '{}' in module '{}'", name.name, module.name),
                                    vec![],
                                ));
                            }
                        } else {
                            return Err(Error::Type(
                                module.span,
                                format!("no module '{}' in scope", module.name),
                                vec![],
                            ));
                        }
                    }
                };
                if generic_count == generic_params.len() {
                    (
                        generic_params
                            .iter()
                            .map(|ty| self.convert(ty, generics, struct_qual, module_qual))
                            .collect::<Result<_, _>>()?,
                        qual_struct.clone(),
                    )
                } else {
                    return Err(Error::Type(
                        span,
                        format!(
                            "type '{}' expects {} generic parameter{} (found {})",
                            name,
                            generic_count,
                            if generic_count == 1 { "" } else { "s" },
                            generic_params.len()
                        ),
                        vec![],
                    ));
                }
            };

        let depth = stars.map_or(0, |s| s.name.len());
        let gtype = match name {
            "int" => GType::Int(depth),
            "float" => GType::Float(depth),
            "byte" => GType::Byte(depth),
            "bool" => GType::Bool(depth),
            _ => {
                if let Some(index) = generics.iter().position(|g| g.name == name) {
                    GType::Generic(depth, index)
                } else {
                    GType::Custom(depth, qualified, generic_params)
                }
            }
        };
        Ok(self.get_or_insert(gtype))
    }

    fn convert_fn_ptr(
        &mut self,
        stars: Option<Name>,
        params: &[PType],
        returns: &[PType],
        generics: &[Name],
        struct_qual: &HashMap<&'static str, Vec<&'static str>>,
        module_qual: &HashMap<&'static str, Vec<&'static str>>,
    ) -> Result<GTypeId, Error> {
        let depth = stars.map_or(0, |s| s.name.len());
        let params = params
            .iter()
            .map(|param| self.convert(param, generics, struct_qual, module_qual))
            .collect::<Result<Vec<_>, _>>()?;
        let returns = returns
            .iter()
            .map(|ret| self.convert(ret, generics, struct_qual, module_qual))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(self.get_or_insert(GType::FnPtr(depth, params, returns)))
    }

    pub fn nonptr_custom_name(&self, id: GTypeId) -> Option<&Vec<&'static str>> {
        match self.gtypes.get_by_right(&id).unwrap() {
            GType::Custom(0, name, _) => Some(name),
            _ => None,
        }
    }

    pub fn substitute(&mut self, id: GTypeId, binds: &[GTypeId]) -> GTypeId {
        let gtype = match self.gtypes.get_by_right(&id).unwrap() {
            GType::Int(depth) => GType::Int(*depth),
            GType::Float(depth) => GType::Float(*depth),
            GType::Byte(depth) => GType::Byte(*depth),
            GType::Bool(depth) => GType::Bool(*depth),
            GType::Custom(depth, name, generics) => GType::Custom(
                *depth,
                name.clone(),
                generics
                    .clone()
                    .iter()
                    .map(|id| self.substitute(*id, binds))
                    .collect(),
            ),
            GType::Generic(depth, index) => self
                .gtypes
                .get_by_right(&binds[*index])
                .unwrap()
                .clone()
                .ref_n(*depth),
            GType::FnPtr(depth, params, returns) => {
                let params = params.clone();
                let returns = returns.clone();
                GType::FnPtr(
                    *depth,
                    params
                        .into_iter()
                        .map(|p| self.substitute(p, binds))
                        .collect(),
                    returns
                        .into_iter()
                        .map(|r| self.substitute(r, binds))
                        .collect(),
                )
            }
        };
        self.get_or_insert(gtype)
    }

    pub fn substitute_concrete(&mut self, id: GTypeId, concrete: &[TypeId]) -> TypeId {
        let ctype = match self.gtypes.get_by_right(&id).unwrap() {
            GType::Int(depth) => Type::Int(*depth),
            GType::Float(depth) => Type::Float(*depth),
            GType::Byte(depth) => Type::Byte(*depth),
            GType::Bool(depth) => Type::Bool(*depth),
            GType::Custom(depth, name, generics) => Type::Custom(
                *depth,
                name.clone(),
                generics
                    .clone()
                    .iter()
                    .map(|id| self.substitute_concrete(*id, concrete))
                    .collect(),
            ),
            GType::Generic(depth, index) => self
                .types
                .get_by_right(&concrete[*index])
                .unwrap()
                .clone()
                .ref_n(*depth),
            GType::FnPtr(depth, params, returns) => {
                let params = params.clone();
                let returns = returns.clone();
                Type::FnPtr(
                    *depth,
                    params
                        .into_iter()
                        .map(|param| self.substitute_concrete(param, concrete))
                        .collect(),
                    returns
                        .into_iter()
                        .map(|ret| self.substitute_concrete(ret, concrete))
                        .collect(),
                )
            }
        };
        self.get_or_insert_concrete(ctype)
    }

    pub fn bind(&mut self, a: GTypeId, b: GTypeId) -> Result<HashMap<usize, GTypeId>, ()> {
        match (
            self.gtypes.get_by_right(&a).unwrap(),
            self.gtypes.get_by_right(&b).unwrap(),
        ) {
            (GType::Int(a_depth), GType::Int(b_depth))
            | (GType::Float(a_depth), GType::Float(b_depth))
            | (GType::Byte(a_depth), GType::Byte(b_depth))
            | (GType::Bool(a_depth), GType::Bool(b_depth)) => {
                if a_depth == b_depth {
                    Ok(HashMap::new())
                } else {
                    Err(())
                }
            }
            (
                GType::Custom(a_depth, a_name, a_generics),
                GType::Custom(b_depth, b_name, b_generics),
            ) => {
                let (a_depth, a_name, a_generics) = (*a_depth, a_name.clone(), a_generics.clone());
                let (b_depth, b_name, b_generics) = (*b_depth, b_name.clone(), b_generics.clone());
                if a_depth == b_depth && a_name == b_name {
                    let mut map = HashMap::new();
                    for (a_id, b_id) in a_generics.into_iter().zip(b_generics) {
                        for (index, id) in self.bind(a_id, b_id)? {
                            match map.entry(index) {
                                hash_map::Entry::Occupied(o) => {
                                    if *o.get() != id {
                                        return Err(());
                                    }
                                }
                                hash_map::Entry::Vacant(v) => {
                                    v.insert(id);
                                }
                            }
                        }
                    }
                    Ok(map)
                } else {
                    Err(())
                }
            }
            (GType::Generic(depth, index), b) => {
                if let Some(ty) = b.clone().deref_n(*depth) {
                    Ok(HashMap::from([(*index, self.get_or_insert(ty))]))
                } else {
                    Err(())
                }
            }
            (
                GType::FnPtr(a_depth, a_params, a_returns),
                GType::FnPtr(b_depth, b_params, b_returns),
            ) => {
                if a_depth != b_depth
                    || a_params.len() != b_params.len()
                    || a_returns.len() != b_returns.len()
                {
                    return Err(());
                }
                let (a_params, a_returns) = (a_params.clone(), a_returns.clone());
                let (b_params, b_returns) = (b_params.clone(), b_returns.clone());
                let mut map = HashMap::new();
                for (a_id, b_id) in a_params.into_iter().zip(b_params) {
                    for (index, id) in self.bind(a_id, b_id)? {
                        match map.entry(index) {
                            hash_map::Entry::Occupied(o) => {
                                if *o.get() != id {
                                    return Err(());
                                }
                            }
                            hash_map::Entry::Vacant(v) => {
                                v.insert(id);
                            }
                        }
                    }
                }
                for (a_id, b_id) in a_returns.into_iter().zip(b_returns) {
                    for (index, id) in self.bind(a_id, b_id)? {
                        match map.entry(index) {
                            hash_map::Entry::Occupied(o) => {
                                if *o.get() != id {
                                    return Err(());
                                }
                            }
                            hash_map::Entry::Vacant(v) => {
                                v.insert(id);
                            }
                        }
                    }
                }
                Ok(map)
            }
            _ => Err(()),
        }
    }

    pub fn int(&mut self) -> GTypeId {
        self.get_or_insert(GType::Int(0))
    }

    pub fn concrete_int(&mut self) -> TypeId {
        self.get_or_insert_concrete(Type::Int(0))
    }

    pub fn float(&mut self) -> GTypeId {
        self.get_or_insert(GType::Float(0))
    }

    pub fn concrete_float(&mut self) -> TypeId {
        self.get_or_insert_concrete(Type::Float(0))
    }

    pub fn byte(&mut self) -> GTypeId {
        self.get_or_insert(GType::Byte(0))
    }

    pub fn concrete_byte(&mut self) -> TypeId {
        self.get_or_insert_concrete(Type::Byte(0))
    }

    pub fn bool(&mut self) -> GTypeId {
        self.get_or_insert(GType::Bool(0))
    }

    pub fn concrete_bool(&mut self) -> TypeId {
        self.get_or_insert_concrete(Type::Bool(0))
    }

    pub fn byte_ptr(&mut self) -> GTypeId {
        self.get_or_insert(GType::Byte(1))
    }

    pub fn byte_ptr_ptr(&mut self) -> GTypeId {
        self.get_or_insert(GType::Byte(2))
    }

    pub fn ref_n(&mut self, id: GTypeId, n: usize) -> GTypeId {
        let ty = self.gtypes.get_by_right(&id).unwrap().clone().ref_n(n);
        self.get_or_insert(ty)
    }

    pub fn depth(&self, id: GTypeId) -> usize {
        self.gtypes.get_by_right(&id).unwrap().depth()
    }

    pub fn concrete_depth(&self, id: TypeId) -> usize {
        self.types.get_by_right(&id).unwrap().depth()
    }

    pub fn make_fn_ptr_type(&mut self, params: Vec<GTypeId>, returns: Vec<GTypeId>) -> GTypeId {
        self.get_or_insert(GType::FnPtr(0, params, returns))
    }

    // TODO: improve this
    pub fn sort_structs(&mut self, structs: &mut [(String, usize, Vec<TypeId>)]) {
        let mut graph = Graph::new();
        let mut map = HashMap::new();
        macro_rules! get_id {
            ($x:expr) => {
                if let Some(id) = map.get(&$x) {
                    *id
                } else {
                    let id = graph.add_node($x);
                    map.insert($x, id);
                    id
                }
            };
        }
        fn first_part(code: &str) -> &str {
            let mut seen = false;
            let mut len = 0;
            for c in code.chars() {
                if c == ' ' {
                    if seen {
                        len += 1;
                        return &code[..len];
                    } else {
                        seen = true;
                    }
                } else {
                    len += c.len_utf8();
                }
            }
            unreachable!()
        }
        fn type_part(line: &str) -> Option<&str> {
            if line.starts_with("    struct") {
                let ty = first_part(&line[4..]);
                if ty.ends_with('*') {
                    None
                } else {
                    Some(ty)
                }
            } else {
                None
            }
        }
        for (code, _, _) in structs.iter() {
            let from = get_id!(first_part(code));
            for line in code.lines().skip(1) {
                if let Some(ty_part) = type_part(line) {
                    let to = get_id!(ty_part);
                    graph.add_edge(from, to, ());
                }
            }
        }
        let sort = toposort(&graph, None).unwrap();
        let typeid_map: HashMap<_, _> = sort
            .iter()
            .rev()
            .enumerate()
            .map(|(i, id)| (graph[*id].to_string(), i))
            .collect();
        structs.sort_by_key(|(code, _, _)| typeid_map[first_part(code)]);
    }

    pub fn generic_indices(&self, id: GTypeId) -> HashSet<usize> {
        match self.gtypes.get_by_right(&id).unwrap() {
            GType::Int(_) | GType::Float(_) | GType::Byte(_) | GType::Bool(_) => HashSet::new(),
            GType::Custom(_, _, generics) => {
                let mut indices = HashSet::new();
                for id in generics {
                    indices.extend(self.generic_indices(*id));
                }
                indices
            }
            GType::Generic(_, index) => HashSet::from([*index]),
            GType::FnPtr(_, params, returns) => {
                let mut indices = HashSet::new();
                for param in params {
                    indices.extend(self.generic_indices(*param));
                }
                for ret in returns {
                    indices.extend(self.generic_indices(*ret));
                }
                indices
            }
        }
    }

    pub fn generic_indices_struct(&self, id: GTypeId, name: &[&str]) -> HashSet<usize> {
        match self.gtypes.get_by_right(&id).unwrap() {
            GType::Int(_) | GType::Float(_) | GType::Byte(_) | GType::Bool(_) => HashSet::new(),
            GType::Custom(_, cname, generics) => {
                if cname == name {
                    HashSet::new()
                } else {
                    let mut indices = HashSet::new();
                    for id in generics {
                        indices.extend(self.generic_indices_struct(*id, name));
                    }
                    indices
                }
            }
            GType::Generic(_, index) => HashSet::from([*index]),
            GType::FnPtr(_, params, returns) => {
                let mut indices = HashSet::new();
                for param in params {
                    indices.extend(self.generic_indices(*param));
                }
                for ret in returns {
                    indices.extend(self.generic_indices(*ret));
                }
                indices
            }
        }
    }

    pub fn generate(&self, id: TypeId) -> String {
        match self.types.get_by_right(&id).unwrap() {
            Type::Int(depth) => format!("int64_t{}", "*".repeat(*depth)),
            Type::Float(depth) => format!("double{}", "*".repeat(*depth)),
            Type::Byte(depth) => format!("uint8_t{}", "*".repeat(*depth)),
            Type::Bool(depth) => format!("uint8_t{}", "*".repeat(*depth)),
            Type::Custom(depth, name, generics) => {
                format!(
                    "struct hs_{}_{}{}",
                    codegen::escape_names(name),
                    self.custom_map[name][generics],
                    "*".repeat(*depth)
                )
            }
            Type::FnPtr(_, _, _) => format!("hfp_{}", id.0),
        }
    }

    pub fn generate_fn_ptr_typedefs(&mut self) -> String {
        let mut graph = Graph::new();
        let mut map = HashMap::new();
        let mut fn_ptrs = Vec::new();
        macro_rules! get_id {
            ($type_id:expr) => {
                if let Some(id) = map.get($type_id) {
                    *id
                } else {
                    let id = graph.add_node($type_id);
                    map.insert(*$type_id, id);
                    id
                }
            };
        }
        for (ty, id) in &self.types {
            if let Type::FnPtr(_, params, returns) = ty {
                let i = get_id!(id);
                for param in params {
                    let j = get_id!(param);
                    graph.add_edge(i, j, ());
                }
                for ret in returns {
                    let j = get_id!(ret);
                    graph.add_edge(i, j, ());
                }
                fn_ptrs.push((ty, id));
            }
        }
        let sort = toposort(&graph, None).unwrap();
        let typeid_map: HashMap<_, _> = sort
            .iter()
            .rev()
            .enumerate()
            .map(|(i, id)| (*graph[*id], i))
            .collect();
        fn_ptrs.sort_by_key(|(_, id)| typeid_map[*id]);

        let mut out = String::new();
        for (ty, id) in fn_ptrs {
            if let Type::FnPtr(depth, params, returns) = ty {
                out.push_str("typedef void (*");
                out.push_str(&"*".repeat(*depth));
                out.push_str(&format!("hfp_{})(", id.0));
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        out.push_str(", ");
                    }
                    out.push_str(&self.generate(*param));
                }
                for (i, ret) in returns.iter().enumerate() {
                    if i > 0 || !params.is_empty() {
                        out.push_str(", ");
                    }
                    out.push_str(&self.generate(*ret));
                    out.push('*');
                }
                out.push_str(");\n");
            }
        }
        out
    }

    pub fn is_fn_ptr_concrete(&self, id: TypeId) -> bool {
        matches!(self.types.get_by_right(&id).unwrap(), Type::FnPtr(_, _, _))
    }

    pub fn format(
        &self,
        id: GTypeId,
        struct_qual: &HashMap<&str, Vec<&str>>,
        module_qual: &HashMap<&str, Vec<&str>>,
    ) -> String {
        match self.gtypes.get_by_right(&id).unwrap() {
            GType::Int(depth) => format!("{}int", "*".repeat(*depth)),
            GType::Float(depth) => format!("{}float", "*".repeat(*depth)),
            GType::Byte(depth) => format!("{}byte", "*".repeat(*depth)),
            GType::Bool(depth) => format!("{}bool", "*".repeat(*depth)),
            GType::Custom(depth, name, generics) => {
                let mut nname = None;
                for (alias, qname) in struct_qual {
                    if name == qname {
                        nname = Some(alias.to_string());
                        break;
                    }
                }
                if nname.is_none() {
                    for qualified in module_qual.values() {
                        if name.len() == qualified.len() + 1 && name.starts_with(qualified) {
                            nname = Some(format!(
                                "{}::{}",
                                name[name.len() - 2],
                                name[name.len() - 1]
                            ));
                            break;
                        }
                    }
                }
                let nname = nname.unwrap_or_else(|| name.join("::"));
                if generics.is_empty() {
                    format!("{}{}", "*".repeat(*depth), nname)
                } else {
                    format!(
                        "{}{}{}",
                        "*".repeat(*depth),
                        nname,
                        self.format_types(generics, struct_qual, module_qual)
                    )
                }
            }
            GType::Generic(depth, index) => format!("{}T{}", "*".repeat(*depth), index),
            GType::FnPtr(depth, params, returns) => {
                let mut out = "*".repeat(*depth);
                out.push_str("fn(");
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        out.push(' ');
                    }
                    out.push_str(&self.format(*param, struct_qual, module_qual));
                }
                if !returns.is_empty() {
                    if params.is_empty() {
                        out.push_str("->");
                    } else {
                        out.push_str(" ->");
                    }
                    for ret in returns {
                        out.push(' ');
                        out.push_str(&self.format(*ret, struct_qual, module_qual));
                    }
                }
                out.push(')');
                out
            }
        }
    }

    pub fn format_types(
        &self,
        types: &[GTypeId],
        struct_qual: &HashMap<&str, Vec<&str>>,
        module_qual: &HashMap<&str, Vec<&str>>,
    ) -> String {
        let mut out = "[".to_string();
        for (i, id) in types.iter().copied().enumerate() {
            if i > 0 {
                out.push_str(", ");
            }
            out.push_str(&self.format(id, struct_qual, module_qual));
        }
        out.push(']');
        out
    }

    pub fn format_stack(
        &self,
        stack: &[(usize, GTypeId)],
        struct_qual: &HashMap<&str, Vec<&str>>,
        module_qual: &HashMap<&str, Vec<&str>>,
    ) -> String {
        let mut out = "[".to_string();
        for (i, (_, id)) in stack.iter().copied().enumerate() {
            if i > 0 {
                out.push_str(", ");
            }
            out.push_str(&self.format(id, struct_qual, module_qual));
        }
        out.push(']');
        out
    }

    pub fn format_signature(
        &self,
        signature: &GSignature,
        struct_qual: &HashMap<&str, Vec<&str>>,
        module_qual: &HashMap<&str, Vec<&str>>,
    ) -> String {
        let mut out = String::new();
        out.push_str(&self.format_types(&signature.params, struct_qual, module_qual));
        out.push_str(" -> ");
        out.push_str(&self.format_types(&signature.returns, struct_qual, module_qual));
        out
    }
}

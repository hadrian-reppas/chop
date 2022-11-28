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
}

impl GSignature {
    pub fn new(params: Vec<GTypeId>, returns: Vec<GTypeId>, kind: Kind) -> GSignature {
        GSignature {
            params,
            returns,
            kind,
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
}

impl GType {
    pub fn ref_n(mut self, n: usize) -> GType {
        match &mut self {
            GType::Int(depth)
            | GType::Float(depth)
            | GType::Byte(depth)
            | GType::Bool(depth)
            | GType::Custom(depth, _, _)
            | GType::Generic(depth, _) => *depth += n,
        }
        self
    }

    pub fn deref_n(mut self, n: usize) -> Option<GType> {
        match &mut self {
            GType::Int(depth) => *depth = depth.checked_sub(n)?,
            GType::Float(depth) => *depth = depth.checked_sub(n)?,
            GType::Byte(depth) => *depth = depth.checked_sub(n)?,
            GType::Bool(depth) => *depth = depth.checked_sub(n)?,
            GType::Custom(depth, _, _) => *depth = depth.checked_sub(n)?,
            GType::Generic(depth, _) => *depth = depth.checked_sub(n)?,
        }
        Some(self)
    }

    pub fn depth(&self) -> usize {
        match self {
            GType::Int(depth) => *depth,
            GType::Float(depth) => *depth,
            GType::Byte(depth) => *depth,
            GType::Bool(depth) => *depth,
            GType::Custom(depth, _, _) => *depth,
            GType::Generic(depth, _) => *depth,
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
}

impl Type {
    pub fn ref_n(mut self, n: usize) -> Type {
        match &mut self {
            Type::Int(depth) => *depth += n,
            Type::Float(depth) => *depth += n,
            Type::Byte(depth) => *depth += n,
            Type::Bool(depth) => *depth += n,
            Type::Custom(depth, _, _) => *depth += n,
        }
        self
    }

    fn depth(&self) -> usize {
        match self {
            Type::Int(depth) => *depth,
            Type::Float(depth) => *depth,
            Type::Byte(depth) => *depth,
            Type::Bool(depth) => *depth,
            Type::Custom(depth, _, _) => *depth,
        }
    }

    fn deref_n(mut self, n: usize) -> Option<Type> {
        match &mut self {
            Type::Int(depth) => *depth = depth.checked_sub(n)?,
            Type::Float(depth) => *depth = depth.checked_sub(n)?,
            Type::Byte(depth) => *depth = depth.checked_sub(n)?,
            Type::Bool(depth) => *depth = depth.checked_sub(n)?,
            Type::Custom(depth, _, _) => *depth = depth.checked_sub(n)?,
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
        todo!();
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
        for (_, id, ftypes) in structs.iter() {
            let from = get_id!(TypeId(*id));
            for ftype in ftypes {
                let depth = self.concrete_depth(*ftype);
                if depth == 0 {
                    let to = get_id!(self.deref_n_concrete(*ftype, depth));
                    graph.add_edge(from, to, ());
                }
            }
        }
        let sort = toposort(&graph, None).unwrap();
        let typeid_map: HashMap<_, _> = sort
            .iter()
            .rev()
            .enumerate()
            .map(|(i, id)| (graph[*id], i))
            .collect();
        structs.sort_by_key(|(_, id, _)| typeid_map[&TypeId(*id)]);
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
        }
    }

    pub fn generic_indices_signature(&self, signature: &GSignature) -> HashSet<usize> {
        let mut indices = HashSet::new();
        for id in &signature.params {
            indices.extend(self.generic_indices(*id));
        }
        indices
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
        }
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

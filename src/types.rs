use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use std::fmt;

// TODO: get rid of TypeId, we only need GTypes
//       Type is for codegen, maybe should live there...

// TODO: consider getting rid of the Types struct, TypeId and GTypeId
// they don't  avoid allocations so no point...

use bimap::BiMap;

use crate::ast::{Item, Name, PType};
use crate::error::{Error, Note};
use crate::lex::Span;
use crate::rust_codegen;

#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(usize);

// TODO: REMOVE THIS
impl fmt::Debug for TypeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "TypeId")
    }
}

#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct GTypeId(usize);

// TODO: REMOVE THIS
impl fmt::Debug for GTypeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "GTypeId")
    }
}

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

    pub fn is_custom(self) -> bool {
        matches!(self, Kind::Custom(_))
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
    Custom(usize, &'static str, Vec<GTypeId>),
    Generic(usize, usize),
}

// TODO: REMOVE
impl GType {
    pub fn ref_n(mut self, n: usize) -> GType {
        match &mut self {
            GType::Int(depth) => *depth += n,
            GType::Float(depth) => *depth += n,
            GType::Byte(depth) => *depth += n,
            GType::Bool(depth) => *depth += n,
            GType::Custom(depth, _, _) => *depth += n,
            GType::Generic(depth, _) => *depth += n,
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
    Custom(usize, &'static str, Vec<TypeId>),
}

// TODO: REMOVE
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
}

pub struct Types {
    types: BiMap<Type, TypeId>,
    gtypes: BiMap<GType, GTypeId>,
    generic_counts: HashMap<&'static str, usize>,
    counter: usize,
}

impl Types {
    pub fn new() -> Types {
        Types {
            types: BiMap::new(),
            gtypes: BiMap::new(),
            generic_counts: HashMap::from([("int", 0), ("float", 0), ("byte", 0), ("bool", 0)]),
            counter: 0,
        }
    }

    pub fn init_generic_counts(&mut self, unit: &[Item]) -> Result<(), Error> {
        let mut struct_spans = HashMap::new();
        for item in unit {
            if let Item::Struct { name, generics, .. } = item {
                if self.generic_counts.contains_key(name.name) {
                    let prev_span: Span = struct_spans[name.name];
                    let note = if prev_span.is_std {
                        let split_path: Vec<_> = prev_span.file.split('/').collect();
                        let path = split_path.join(":");
                        Note::new(None, format!("'{}' is defined in {}", name.name, path))
                    } else {
                        Note::new(Some(prev_span), "previous definition is here".to_string())
                    };
                    return Err(Error::Type(
                        name.span,
                        format!("type '{}' is already defined", name.name),
                        vec![note],
                    ));
                } else {
                    let count = match generics {
                        Some(generics) => generics.names.len(),
                        None => 0,
                    };
                    self.generic_counts.insert(name.name, count);
                    struct_spans.insert(name.name, name.span);
                }
            }
        }
        Ok(())
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
            self.types.insert(ty, TypeId(self.counter));
            self.counter += 1;
            TypeId(self.counter - 1)
        }
    }

    pub fn convert(&mut self, ty: &PType, generics: &[Name]) -> Result<GTypeId, Error> {
        let (name, span) = (ty.name.name, ty.name.span);
        let depth = ty.stars.map(|s| s.name.len()).unwrap_or(0);
        let generic_params = if generics.iter().any(|g| g.name == name) {
            if ty.generics.is_some() {
                return Err(Error::Type(
                    span,
                    "generics cannot have generic parameters".to_string(),
                    vec![],
                ));
            } else {
                Vec::new()
            }
        } else if let Some(generic_count) = self.generic_counts.get(name) {
            if *generic_count == 0 {
                if ty.generics.is_some() {
                    return Err(Error::Type(
                        span,
                        format!("type '{}' does not have generic parameters", name),
                        vec![],
                    ));
                } else {
                    Vec::new()
                }
            } else if let Some(gens) = &ty.generics {
                if gens.types.len() == *generic_count {
                    gens.types
                        .iter()
                        .map(|ty| self.convert(ty, generics))
                        .collect::<Result<_, _>>()?
                } else {
                    return Err(Error::Type(
                        span,
                        format!(
                            "type '{}' expects {} generic parameter{} (found {})",
                            name,
                            generic_count,
                            if *generic_count == 1 { "s" } else { "" },
                            gens.types.len(),
                        ),
                        vec![],
                    ));
                }
            } else {
                return Err(Error::Type(
                    span,
                    format!(
                        "type '{}' expects {} generic parameter{} (found 0)",
                        name,
                        generic_count,
                        if *generic_count == 1 { "s" } else { "" },
                    ),
                    vec![],
                ));
            }
        } else {
            return Err(Error::Type(
                span,
                format!("unknown type '{}'", name),
                vec![],
            ));
        };
        let gtype = match name {
            "int" => GType::Int(depth),
            "float" => GType::Float(depth),
            "byte" => GType::Byte(depth),
            "bool" => GType::Bool(depth),
            _ => {
                if let Some(index) = generics.iter().position(|g| g.name == name) {
                    GType::Generic(depth, index)
                } else {
                    GType::Custom(depth, name, generic_params)
                }
            }
        };
        Ok(self.get_or_insert(gtype))
    }

    pub fn nonptr_custom_name(&self, id: GTypeId) -> Option<&'static str> {
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
                name,
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
                name,
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
                let (a_depth, a_name, a_generics) = (*a_depth, *a_name, a_generics.clone());
                let (b_depth, b_name, b_generics) = (*b_depth, *b_name, b_generics.clone());
                if a_depth == b_depth && a_name == b_name {
                    let mut map = HashMap::new();
                    for (a_id, b_id) in a_generics.into_iter().zip(b_generics) {
                        for (index, id) in self.bind(a_id, b_id)? {
                            match map.entry(index) {
                                Entry::Occupied(o) => {
                                    if *o.get() != id {
                                        return Err(());
                                    }
                                }
                                Entry::Vacant(v) => {
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

    pub fn float(&mut self) -> GTypeId {
        self.get_or_insert(GType::Float(0))
    }

    pub fn byte(&mut self) -> GTypeId {
        self.get_or_insert(GType::Byte(0))
    }

    pub fn bool(&mut self) -> GTypeId {
        self.get_or_insert(GType::Bool(0))
    }

    pub fn byte_ptr(&mut self) -> GTypeId {
        self.get_or_insert(GType::Byte(1))
    }

    pub fn byte_ptr_ptr(&mut self) -> GTypeId {
        self.get_or_insert(GType::Byte(2))
    }

    pub fn overlap(&mut self, a: GTypeId, b: GTypeId) -> bool {
        self.bind(a, b).is_ok()
    }

    pub fn ref_n(&mut self, id: GTypeId, n: usize) -> GTypeId {
        let ty = self.gtypes.get_by_right(&id).unwrap().clone().ref_n(n);
        self.get_or_insert(ty)
    }

    pub fn depth(&self, id: GTypeId) -> usize {
        self.gtypes.get_by_right(&id).unwrap().depth()
    }

    pub fn generic_indices(&self, id: GTypeId) -> HashSet<usize> {
        match self.gtypes.get_by_right(&id).unwrap() {
            GType::Int(_) => HashSet::new(),
            GType::Float(_) => HashSet::new(),
            GType::Byte(_) => HashSet::new(),
            GType::Bool(_) => HashSet::new(),
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

    pub fn generic_indices_struct(&self, id: GTypeId, name: &str) -> HashSet<usize> {
        match self.gtypes.get_by_right(&id).unwrap() {
            GType::Int(_) => HashSet::new(),
            GType::Float(_) => HashSet::new(),
            GType::Byte(_) => HashSet::new(),
            GType::Bool(_) => HashSet::new(),
            GType::Custom(_, cname, generics) => {
                if *cname == name {
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

    pub fn display(&self, id: GTypeId) {
        match self.gtypes.get_by_right(&id).unwrap() {
            GType::Int(depth) => print!("{}Int{}", "Ptr(".repeat(*depth), ")".repeat(*depth)),
            GType::Float(depth) => print!("{}Float{}", "Ptr(".repeat(*depth), ")".repeat(*depth)),
            GType::Byte(depth) => print!("{}Byte{}", "Ptr(".repeat(*depth), ")".repeat(*depth)),
            GType::Bool(depth) => print!("{}Bool{}", "Ptr(".repeat(*depth), ")".repeat(*depth)),
            GType::Custom(depth, name, generics) => {
                print!("{}Custom({:?}, ", "Ptr(".repeat(*depth), name);
                if generics.is_empty() {
                    print!("[]){}", ")".repeat(*depth));
                } else {
                    print!("[");
                    for (i, id) in generics.iter().enumerate() {
                        if i > 0 {
                            print!(", ");
                        }
                        self.display(*id);
                    }
                    print!("]){}", ")".repeat(*depth));
                }
            }
            GType::Generic(depth, index) => print!(
                "{}Gen({}){}",
                "Ptr(".repeat(*depth),
                index,
                ")".repeat(*depth),
            ),
        }
    }

    pub fn display_concrete(&self, id: TypeId) {
        match self.types.get_by_right(&id).unwrap() {
            Type::Int(depth) => print!("{}Int{}", "Ptr(".repeat(*depth), ")".repeat(*depth)),
            Type::Float(depth) => print!("{}Float{}", "Ptr(".repeat(*depth), ")".repeat(*depth)),
            Type::Byte(depth) => print!("{}Byte{}", "Ptr(".repeat(*depth), ")".repeat(*depth)),
            Type::Bool(depth) => print!("{}Bool{}", "Ptr(".repeat(*depth), ")".repeat(*depth)),
            Type::Custom(depth, name, generics) => {
                print!("{}Custom({:?}, ", "Ptr(".repeat(*depth), name);
                if generics.is_empty() {
                    print!("[]){}", ")".repeat(*depth));
                } else {
                    print!("[");
                    for (i, id) in generics.iter().enumerate() {
                        if i > 0 {
                            print!(", ");
                        }
                        self.display_concrete(*id);
                    }
                    print!("]){}", ")".repeat(*depth));
                }
            }
        }
    }

    pub fn format(&self, id: GTypeId) -> String {
        match self.gtypes.get_by_right(&id).unwrap() {
            GType::Int(depth) => format!("{}int", "*".repeat(*depth)),
            GType::Float(depth) => format!("{}float", "*".repeat(*depth)),
            GType::Byte(depth) => format!("{}byte", "*".repeat(*depth)),
            GType::Bool(depth) => format!("{}bool", "*".repeat(*depth)),
            GType::Custom(depth, name, generics) => {
                if generics.is_empty() {
                    format!("{}{}", "*".repeat(*depth), name)
                } else {
                    format!(
                        "{}{}{}",
                        "*".repeat(*depth),
                        name,
                        self.format_types(generics)
                    )
                }
            }
            GType::Generic(depth, index) => format!("{}T{}", "*".repeat(*depth), index),
        }
    }

    pub fn format_types(&self, stack: &[GTypeId]) -> String {
        let mut out = "[".to_string();
        for (i, id) in stack.iter().copied().enumerate() {
            if i > 0 {
                out.push_str(", ");
            }
            out.push_str(&self.format(id));
        }
        out.push(']');
        out
    }

    pub fn format_stack(&self, stack: &[(usize, GTypeId)]) -> String {
        let mut out = "[".to_string();
        for (i, (_, id)) in stack.iter().copied().enumerate() {
            if i > 0 {
                out.push_str(", ");
            }
            out.push_str(&self.format(id));
        }
        out.push(']');
        out
    }

    pub fn format_signature(&self, signature: &GSignature) -> String {
        let mut out = String::new();
        out.push_str(&self.format_types(&signature.params));
        out.push_str(" -> ");
        out.push_str(&self.format_types(&signature.returns));
        out
    }

    pub fn rust_type(&self, id: GTypeId) -> String {
        match self.gtypes.get_by_right(&id).unwrap() {
            GType::Int(depth) => format!("{}i64", "*mut ".repeat(*depth)),
            GType::Float(depth) => format!("{}f64", "*mut ".repeat(*depth)),
            GType::Byte(depth) => format!("{}u8", "*mut ".repeat(*depth)),
            GType::Bool(depth) => format!("{}bool", "*mut ".repeat(*depth)),
            GType::Custom(depth, name, generics) => {
                let mut out = format!("{}hs_", "*mut ".repeat(*depth));
                rust_codegen::escape_name(name, &mut out);
                if !generics.is_empty() {
                    out.push('<');
                    for (i, id) in generics.iter().enumerate() {
                        if i > 0 {
                            out.push_str(", ");
                        }
                        out.push_str(&self.rust_type(*id));
                    }
                    out.push('>');
                }
                out
            }
            GType::Generic(depth, index) => format!("{}T{index}", "*mut ".repeat(*depth)),
        }
    }

    pub fn rust_type_concrete(&self, id: TypeId) -> String {
        match self.types.get_by_right(&id).unwrap() {
            Type::Int(depth) => format!("{}i64", "*mut ".repeat(*depth)),
            Type::Float(depth) => format!("{}f64", "*mut ".repeat(*depth)),
            Type::Byte(depth) => format!("{}u8", "*mut ".repeat(*depth)),
            Type::Bool(depth) => format!("{}bool", "*mut ".repeat(*depth)),
            Type::Custom(depth, name, generics) => {
                let mut out = format!("{}hs_", "*mut ".repeat(*depth));
                rust_codegen::escape_name(name, &mut out);
                if !generics.is_empty() {
                    out.push('<');
                    for (i, id) in generics.iter().enumerate() {
                        if i > 0 {
                            out.push_str(", ");
                        }
                        out.push_str(&self.rust_type_concrete(*id));
                    }
                    out.push('>');
                }
                out
            }
        }
    }
}

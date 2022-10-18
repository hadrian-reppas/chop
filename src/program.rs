use std::collections::{hash_map::Entry, HashMap};

use crate::types::{GTypeId, TypeId};

pub struct ProgramContext {
    pub program: Program,
    pub wip_fn: Option<ProgramFn>,
    pub wip_global: Option<ProgramGlobal>,
    pub vars: HashMap<GTypeId, Vec<usize>>,
    free_vars: HashMap<GTypeId, Vec<usize>>,
    counter: usize,
}

impl ProgramContext {
    pub fn new() -> ProgramContext {
        ProgramContext {
            program: Program::new(),
            wip_fn: None,
            wip_global: None,
            vars: HashMap::new(),
            free_vars: HashMap::new(),
            counter: 0,
        }
    }

    pub fn init_vars(&mut self, params: &[GTypeId]) {
        self.free_vars = HashMap::new();
        self.counter = params.len();
        for (i, id) in params.iter().enumerate() {
            match self.vars.entry(*id) {
                Entry::Occupied(mut o) => o.get_mut().push(i),
                Entry::Vacant(v) => {
                    v.insert(vec![i]);
                }
            }
            self.free_vars.insert(*id, Vec::new());
        }
    }

    pub fn alloc(&mut self, ty: GTypeId) -> usize {
        if let Some(free) = self.free_vars.get_mut(&ty) {
            if let Some(var) = free.pop() {
                var
            } else {
                self.vars.insert(ty, vec![self.counter]);
                self.counter += 1;
                self.counter - 1
            }
        } else {
            self.vars.insert(ty, vec![self.counter]);
            self.free_vars.insert(ty, Vec::new());
            self.counter += 1;
            self.counter - 1
        }
    }

    pub fn free(&mut self, var: usize, ty: GTypeId) {
        self.free_vars.get_mut(&ty).unwrap().push(var);
    }
}

#[derive(Debug)]
pub struct Program {
    pub structs: Vec<ProgramStruct>,
    pub functions: Vec<ProgramFn>,
    pub globals: Vec<ProgramGlobal>,
}

impl Program {
    pub fn new() -> Program {
        Program {
            structs: Vec::new(),
            functions: Vec::new(),
            globals: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub struct ProgramStruct {
    pub generic_count: usize,
    pub name: &'static str,
    pub members: Vec<ProgramMember>,
}

#[derive(Debug)]
pub struct ProgramMember {
    pub name: &'static str,
    pub type_id: GTypeId,
}

#[derive(Debug)]
pub struct ProgramFn {
    pub name: &'static str,
    pub generic_count: usize,
    pub params: Vec<GTypeId>,
    pub returns: Vec<GTypeId>,
    pub body: Vec<ProgramStmt>,
    pub vars: HashMap<GTypeId, Vec<usize>>,
    pub return_vars: Vec<usize>,
}

#[derive(Debug)]
pub struct ProgramGlobal {
    pub name: &'static str,
    pub type_id: TypeId,
    pub init: Option<ProgramGlobalInit>,
}

#[derive(Debug)]
pub struct ProgramGlobalInit {
    pub ops: Vec<ProgramOp>,
    pub var: usize,
    pub vars: HashMap<GTypeId, Vec<usize>>,
}

#[derive(Debug)]
pub enum ProgramOp {
    Int(usize, i64),
    Float(usize, f64),
    Bool(usize, bool),
    String(usize, String),
    Call(&'static str, usize, Vec<usize>, Vec<usize>),
}

#[derive(Debug)]
pub enum ProgramStmt {
    Op(ProgramOp),
    // TODO
}

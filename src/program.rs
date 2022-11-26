use std::collections::{hash_map::Entry, HashMap};
use std::mem;

use crate::{
    lex::Span,
    types::{GTypeId, TypeId},
};

pub struct ProgramContext {
    pub program: Program,
    pub wip_init: Vec<ProgramOp>,
    pub wip_bodies: Vec<Vec<ProgramStmt>>,
    pub vars: HashMap<GTypeId, Vec<usize>>,
    pub if_bodies: Vec<Vec<ProgramStmt>>,
    pub counter: usize,
    state: State,
}

impl ProgramContext {
    pub fn new() -> ProgramContext {
        ProgramContext {
            program: Program::new(),
            wip_init: Vec::new(),
            wip_bodies: Vec::new(),
            vars: HashMap::new(),
            if_bodies: Vec::new(),
            counter: 0,
            state: State::Paused,
        }
    }

    pub fn init_vars(&mut self, params: &[GTypeId]) {
        self.counter = params.len();
        for (i, id) in params.iter().enumerate() {
            match self.vars.entry(*id) {
                Entry::Occupied(mut o) => o.get_mut().push(i),
                Entry::Vacant(v) => {
                    v.insert(vec![i]);
                }
            }
        }
    }

    pub fn alloc(&mut self, ty: GTypeId) -> usize {
        if let Some(vars) = self.vars.get_mut(&ty) {
            vars.push(self.counter);
            self.counter += 1;
            self.counter - 1
        } else {
            self.vars.insert(ty, vec![self.counter]);
            self.counter += 1;
            self.counter - 1
        }
    }

    pub fn pause(&mut self) {
        self.state = State::Paused;
    }

    pub fn resume(&mut self) {
        self.state = State::Fn;
    }

    pub fn push_op(&mut self, op: ProgramOp) {
        match self.state {
            State::Global => self.wip_init.push(op),
            State::Fn => self
                .wip_bodies
                .last_mut()
                .unwrap()
                .push(ProgramStmt::Op(op)),
            State::Paused => {}
        }
    }

    pub fn begin_if(&mut self) {
        self.begin_body();
    }

    pub fn begin_else(&mut self) {
        self.if_bodies
            .push(mem::take(self.wip_bodies.last_mut().unwrap()));
    }

    pub fn end_if_else(&mut self, test_var: usize, resolve: Vec<(usize, usize, GTypeId)>) {
        let else_body = self.wip_bodies.pop().unwrap();
        let body = self.if_bodies.pop().unwrap();
        self.wip_bodies.last_mut().unwrap().push(ProgramStmt::If {
            test_var,
            body,
            else_body,
            resolve,
        });
    }

    pub fn begin_for(&mut self) {
        self.begin_body();
    }

    pub fn end_for(
        &mut self,
        iter_var: usize,
        low_var: usize,
        high_var: usize,
        resolve: Vec<(usize, usize, GTypeId)>,
    ) {
        let body = self.wip_bodies.pop().unwrap();
        self.wip_bodies.last_mut().unwrap().push(ProgramStmt::For {
            iter_var,
            low_var,
            high_var,
            body,
            resolve,
        });
    }

    pub fn begin_while(&mut self) {
        self.begin_body();
    }

    pub fn end_while(&mut self, test_var: usize, resolve: Vec<(usize, usize, GTypeId)>) {
        let body = self.wip_bodies.pop().unwrap();
        self.wip_bodies
            .last_mut()
            .unwrap()
            .push(ProgramStmt::While {
                test_var,
                body,
                resolve,
            });
    }

    pub fn begin_body(&mut self) {
        self.wip_bodies.push(Vec::new());
    }

    pub fn begin_fn(&mut self, params: &[GTypeId]) {
        self.state = State::Fn;
        self.init_vars(params);
        self.begin_body();
    }

    pub fn end_fn(
        &mut self,
        stack: &[(usize, GTypeId)],
        name: Vec<&'static str>,
        index: usize,
        generic_count: usize,
        params: Vec<GTypeId>,
        returns: Vec<GTypeId>,
    ) {
        let vars: HashMap<_, _> = self
            .vars
            .drain()
            .map(|(id, vars)| {
                (
                    id,
                    vars.into_iter()
                        .filter(|var| !(0..params.len()).contains(var))
                        .collect::<Vec<_>>(),
                )
            })
            .filter(|(_, vars)| !vars.is_empty())
            .collect();
        let body = self.wip_bodies.pop().unwrap();
        self.program.functions.push(ProgramFn {
            name,
            index,
            generic_count,
            params,
            returns,
            body,
            vars,
            return_vars: stack.iter().map(|(var, _)| *var).collect(),
        });
    }

    pub fn begin_global(&mut self) {
        self.state = State::Global;
        self.init_vars(&[]);
    }

    pub fn end_global(&mut self, name: Vec<&'static str>, type_id: TypeId) {
        self.program.globals.push(ProgramGlobal {
            name,
            type_id,
            init: None,
        });
    }

    pub fn end_global_init(&mut self, name: Vec<&'static str>, type_id: TypeId, var: usize) {
        self.program.globals.push(ProgramGlobal {
            name,
            type_id,
            init: Some(ProgramGlobalInit {
                ops: mem::take(&mut self.wip_init),
                var,
                vars: mem::take(&mut self.vars),
            }),
        });
    }
}

enum State {
    Global,
    Fn,
    Paused,
}

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

pub struct ProgramStruct {
    pub generic_count: usize,
    pub name: &'static str,
    pub members: Vec<ProgramMember>,
}

pub struct ProgramMember {
    pub name: &'static str,
    pub type_id: GTypeId,
}

pub struct ProgramFn {
    pub name: Vec<&'static str>,
    pub index: usize,
    pub generic_count: usize,
    pub params: Vec<GTypeId>,
    pub returns: Vec<GTypeId>,
    pub body: Vec<ProgramStmt>,
    pub vars: HashMap<GTypeId, Vec<usize>>,
    pub return_vars: Vec<usize>,
}

#[derive(Clone)]
pub struct ProgramGlobal {
    pub name: Vec<&'static str>,
    pub type_id: TypeId,
    pub init: Option<ProgramGlobalInit>,
}

#[derive(Clone)]
pub struct ProgramGlobalInit {
    pub ops: Vec<ProgramOp>,
    pub var: usize,
    pub vars: HashMap<GTypeId, Vec<usize>>,
}

#[derive(Clone)]
pub enum ProgramOp {
    Int(usize, i64),
    Float(usize, f64),
    Bool(usize, bool),
    Byte(usize, u8),
    String(usize, String),
    Call(
        Vec<&'static str>,
        usize,
        Vec<usize>,
        Vec<usize>,
        Vec<GTypeId>,
    ),
    Ref(usize, usize, GTypeId),
    Assert(usize, Span),
    Abort(Span),
    SizeOf(usize, GTypeId),
    Alloc(usize, GTypeId),
    Zalloc(usize, GTypeId),
    AllocArr(usize, usize, GTypeId),
    ZallocArr(usize, usize, GTypeId),
    CastTo(usize, usize, GTypeId),
}

pub enum ProgramStmt {
    Op(ProgramOp),
    If {
        test_var: usize,
        body: Vec<ProgramStmt>,
        else_body: Vec<ProgramStmt>,
        resolve: Vec<(usize, usize, GTypeId)>,
    },
    For {
        iter_var: usize,
        low_var: usize,
        high_var: usize,
        body: Vec<ProgramStmt>,
        resolve: Vec<(usize, usize, GTypeId)>,
    },
    While {
        test_var: usize,
        body: Vec<ProgramStmt>,
        resolve: Vec<(usize, usize, GTypeId)>,
    },
}

use std::collections::{hash_map::Entry, HashMap, HashSet};
use std::mem;

use crate::{
    lex::Span,
    types::{GTypeId, TypeId, Types},
};

pub struct ProgramContext {
    pub program: Program,
    pub wip_init: Vec<ProgramOp>,
    pub wip_bodies: Vec<Vec<ProgramStmt>>,
    pub vars: HashMap<GTypeId, Vec<usize>>,
    pub free_vars: HashMap<GTypeId, Vec<usize>>,
    pub if_bodies: Vec<Vec<ProgramStmt>>,
    pub counter: usize,
    pub let_counts: HashMap<usize, usize>,
    pub for_vars: HashSet<usize>,
    pub to_free: HashSet<usize>,
    state: State,
}

impl ProgramContext {
    pub fn new() -> ProgramContext {
        ProgramContext {
            program: Program::new(),
            wip_init: Vec::new(),
            wip_bodies: Vec::new(),
            vars: HashMap::new(),
            free_vars: HashMap::new(),
            if_bodies: Vec::new(),
            counter: 0,
            let_counts: HashMap::new(),
            for_vars: HashSet::new(),
            to_free: HashSet::new(),
            state: State::Paused,
        }
    }

    pub fn init_vars(&mut self, params: &[GTypeId]) {
        self.free_vars = HashMap::new();
        self.let_counts = HashMap::new();
        self.to_free = HashSet::new();
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
        if let Some(vars) = self.vars.get_mut(&ty) {
            if let Some(var) = self.free_vars.get_mut(&ty).unwrap().pop() {
                var
            } else {
                vars.push(self.counter);
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
        if self.let_counts.contains_key(&var) {
            self.to_free.insert(var);
        } else if !self.for_vars.contains(&var) {
            self.free_vars.get_mut(&ty).unwrap().push(var);
        }
    }

    pub fn alloc_let(&mut self, var: usize) {
        if let Some(count) = self.let_counts.get(&var) {
            self.let_counts.insert(var, count + 1);
        } else {
            self.let_counts.insert(var, 1);
        }
    }

    pub fn free_let(&mut self, var: usize, ty: GTypeId) {
        let count = self.let_counts[&var];
        if count == 1 {
            self.let_counts.remove(&var);
            if self.to_free.contains(&var) {
                self.to_free.remove(&var);
                self.free_vars.get_mut(&ty).unwrap().push(var);
            }
        } else {
            self.let_counts.insert(var, count - 1);
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

    pub fn begin_else(&mut self, stack: &[(usize, GTypeId)], else_stack: &[(usize, GTypeId)]) {
        self.if_bodies
            .push(mem::take(self.wip_bodies.last_mut().unwrap()));
        let on_stack: HashSet<_> = else_stack.iter().map(|(var, _)| *var).collect();
        for (var, ty) in stack {
            if !on_stack.contains(var) {
                self.free(*var, *ty);
            }
        }
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

    pub fn display(&self, types: &Types) {
        for global in &self.globals {
            global.display(types);
        }
        for struct_def in &self.structs {
            struct_def.display(types);
        }
        for function in &self.functions {
            function.display(types);
        }
    }
}

#[derive(Debug)]
pub struct ProgramStruct {
    pub generic_count: usize,
    pub name: &'static str,
    pub members: Vec<ProgramMember>,
}

impl ProgramStruct {
    fn display(&self, types: &Types) {
        print!("Struct({:?}, {}, ", self.name, self.generic_count);
        if self.members.is_empty() {
            println!("[])");
        } else {
            println!("[");
            for member in &self.members {
                print!("    ({:?}, ", member.name);
                types.display(member.type_id);
                println!("),");
            }
            println!("])");
        }
    }
}

#[derive(Debug)]
pub struct ProgramMember {
    pub name: &'static str,
    pub type_id: GTypeId,
}

#[derive(Debug)]
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

impl ProgramFn {
    fn display(&self, types: &Types) {
        println!(
            "Function({:?}, {}, {},",
            self.name, self.index, self.generic_count,
        );
        print!("    [");
        for (i, type_id) in self.params.iter().enumerate() {
            if i > 0 {
                print!(", ");
            }
            types.display(*type_id);
        }
        println!("],");
        print!("    [");
        for (i, type_id) in self.returns.iter().enumerate() {
            if i > 0 {
                print!(", ");
            }
            types.display(*type_id);
        }
        println!("],");
        display_vars(&self.vars, types);
        if self.body.is_empty() {
            println!("    [],");
        } else {
            println!("    [");
            for stmt in &self.body {
                stmt.display(types, 2);
            }
            println!("    ],");
        }
        println!("    {:?},\n)", self.return_vars);
    }
}

#[derive(Debug, Clone)]
pub struct ProgramGlobal {
    pub name: Vec<&'static str>,
    pub type_id: TypeId,
    pub init: Option<ProgramGlobalInit>,
}

impl ProgramGlobal {
    fn display(&self, types: &Types) {
        print!("Global({:?}, ", self.name);
        types.display_concrete(self.type_id);
        if let Some(init) = &self.init {
            print!(", ");
            init.display(types);
            println!(")");
        } else {
            println!(", None)");
        }
    }
}

#[derive(Debug, Clone)]
pub struct ProgramGlobalInit {
    pub ops: Vec<ProgramOp>,
    pub var: usize,
    pub vars: HashMap<GTypeId, Vec<usize>>,
}

impl ProgramGlobalInit {
    fn display(&self, types: &Types) {
        println!("Init(");
        display_vars(&self.vars, types);
        println!("    [");
        for op in &self.ops {
            op.display(types, 2);
            println!(",");
        }
        println!("    ],");
        print!("    {},\n)", self.var);
    }
}

fn display_vars(vars: &HashMap<GTypeId, Vec<usize>>, types: &Types) {
    if vars.is_empty() {
        println!("    Vars([]),");
    } else {
        println!("    Vars([");
        for (id, vars) in vars {
            print!("        (");
            types.display(*id);
            println!(", {:?}),", vars);
        }
        println!("    ]),");
    }
}

#[derive(Debug, Clone)]
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

impl ProgramOp {
    fn display(&self, types: &Types, depth: usize) {
        print!("{}", "    ".repeat(depth));
        match self {
            ProgramOp::Int(var, val) => print!("IntLit({var}, {val})"),
            ProgramOp::Float(var, val) => print!("FloatLit({var}, {val:?})"),
            ProgramOp::Bool(var, val) => print!("BoolLit({var}, {val})"),
            ProgramOp::Byte(var, val) => print!("ByteLit({var}, {val})"),
            ProgramOp::String(var, val) => print!("StringLit({var}, {val:?})"),
            ProgramOp::Call(name, index, params, returns, _) => {
                print!("Call({name:?}, {index}, {params:?}, {returns:?})");
            }
            ProgramOp::Ref(var, old_var, type_id) => {
                print!("Ref({var}, {old_var}, ");
                types.display(*type_id);
                print!(")");
            }
            ProgramOp::Assert(var, _) => print!("Assert({var})"),
            ProgramOp::Abort(_) => print!("Abort"),
            ProgramOp::SizeOf(var, type_id) => {
                print!("SizeOf({var}, ");
                types.display(*type_id);
                print!(")");
            }
            ProgramOp::Alloc(var, type_id) => {
                print!("Alloc({var}, ");
                types.display(*type_id);
                print!(")");
            }
            ProgramOp::Zalloc(var, type_id) => {
                print!("Zalloc({var}, ");
                types.display(*type_id);
                print!(")");
            }
            ProgramOp::AllocArr(var, len_var, type_id) => {
                print!("AllocArr({var}, {len_var}, ");
                types.display(*type_id);
                print!(")");
            }
            ProgramOp::ZallocArr(var, len_var, type_id) => {
                print!("ZallocArr({var}, {len_var}, ");
                types.display(*type_id);
                print!(")");
            }
            ProgramOp::CastTo(var, old_var, type_id) => {
                print!("CastTo({var}, {old_var}, ");
                types.display(*type_id);
                print!(")");
            }
        }
    }
}

#[derive(Debug)]
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

impl ProgramStmt {
    fn display(&self, types: &Types, depth: usize) {
        print!("{}", "    ".repeat(depth));
        match self {
            ProgramStmt::Op(op) => {
                op.display(types, 0);
                println!(",");
            }
            ProgramStmt::If {
                test_var,
                body,
                else_body,
                resolve,
            } => {
                print!("If({test_var}, [");
                if body.is_empty() {
                    println!("],");
                } else {
                    println!();
                    for stmt in body {
                        stmt.display(types, depth + 2);
                    }
                    println!("{}],", "    ".repeat(depth + 1));
                }
                print!("{}[", "    ".repeat(depth + 1));
                if else_body.is_empty() {
                    println!("],");
                } else {
                    println!();
                    for stmt in else_body {
                        stmt.display(types, depth + 2);
                    }
                    println!("{}],", "    ".repeat(depth + 1));
                }
                println!("{}{resolve:?},", "    ".repeat(depth + 1));
                println!("{}),", "    ".repeat(depth));
            }
            ProgramStmt::For {
                iter_var,
                low_var,
                high_var,
                body,
                resolve,
            } => {
                println!("For({iter_var}, {low_var}, {high_var}, [");
                for stmt in body {
                    stmt.display(types, depth + 2);
                }
                println!("{}],", "    ".repeat(depth + 1));
                println!("{}{resolve:?},", "    ".repeat(depth + 1));
                println!("{}),", "    ".repeat(depth));
            }
            ProgramStmt::While {
                test_var,
                body,
                resolve,
            } => {
                print!("While({test_var}, [");
                if body.is_empty() {
                    println!("],");
                } else {
                    println!();
                    for stmt in body {
                        stmt.display(types, depth + 2);
                    }
                    println!("{}],", "    ".repeat(depth + 1));
                }
                println!("{}{resolve:?},", "    ".repeat(depth + 1));
                println!("{}),", "    ".repeat(depth));
            }
        }
    }
}

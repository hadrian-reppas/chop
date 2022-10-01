use crate::ast::{ElsePart, Expr, Name, Op, Stmt};
use crate::typecheck::{GSignature, GType, Kind, Primitive, TypeInfo};

use std::collections::{hash_map, HashMap, HashSet};
use std::fmt;

//                       name                 id     concrete signatures
type Concrete = HashMap<&'static str, HashMap<usize, Vec<Signature>>>;
type Done = HashSet<(&'static str, usize, Vec<Type>)>;

pub fn generate(info: &TypeInfo) -> String {
    let mut context = Context::new(info);
    context.make();
    context.code
}

const PRELUDE: &str = "
#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include <wchar.h>
#include <locale.h>
";

struct Context<'info> {
    code: String,
    info: &'info TypeInfo,
    stack: Vec<(usize, Type)>,
    concrete: Concrete,
    done: Done,
    counter: usize,
    depth: usize,
    let_binds: Vec<HashMap<&'static str, (usize, Type)>>,
}

impl<'info> Context<'info> {
    fn new(info: &'info TypeInfo) -> Context {
        Context {
            code: PRELUDE.to_string(),
            info,
            stack: Vec::new(),
            concrete: HashMap::new(),
            done: HashSet::new(),
            counter: 0,
            depth: 1,
            let_binds: Vec::new(),
        }
    }

    fn make(&mut self) {
        self.fill_graph("main", 0, Vec::new());

        let mut functions = Vec::new();
        for (name, signatures) in &self.info.signatures {
            for (gid, GSignature { kind, .. }) in signatures.iter().enumerate() {
                if let Some(signatures) = self.concrete.get(name) {
                    if let Some(signatures) = signatures.get(&gid) {
                        for (cid, signature) in signatures.iter().enumerate() {
                            functions.push((*name, gid, cid, signature.clone(), kind.clone()));
                        }
                    }
                }
            }
        }

        for (name, gid, cid, signature, _) in &functions {
            self.make_declaration(name, *gid, *cid, signature);
        }

        for (name, gid, cid, signature, kind) in functions {
            self.make_function(name, gid, cid, &signature, &kind);
        }

        let main_signature = self.concrete.get("main").unwrap().get(&0).unwrap()[0].clone();
        self.make_main(&main_signature);
    }

    fn make_main(&mut self, main_signature: &Signature) {
        self.add("int main(int argc, char** argv) {\n");
        self.add("  setlocale(LC_ALL, \"\");\n");
        self.add("  int64_t code = 0;\n");
        self.add("  hopf_6d_61_69_6e_0_0(");
        if !main_signature.params.is_empty() {
            self.add("argc, (uint8_t**) argv");
        }
        if !main_signature.returns.is_empty() {
            if !main_signature.params.is_empty() {
                self.add(", ");
            }
            self.add("&code");
        }
        self.add(");\n  return code;\n}\n");
    }

    fn push(&mut self, ty: Type) -> usize {
        self.stack.push((self.counter, ty));
        self.counter += 1;
        self.counter - 1
    }

    fn var(&mut self) -> usize {
        self.counter += 1;
        self.counter - 1
    }

    fn get_signature(&self, signature: &GSignature) -> Option<Signature> {
        let bindings = self.get_bindings(signature)?;
        Some(Signature {
            params: signature
                .params
                .iter()
                .map(|param| param.substitute_(&bindings))
                .collect(),
            returns: signature
                .returns
                .iter()
                .map(|param| param.substitute_(&bindings))
                .collect(),
        })
    }

    fn get_bindings(&self, signature: &GSignature) -> Option<Vec<Type>> {
        let mut bindings = vec![None; signature.params.len()];
        for ((_, arg), param) in self.stack.iter().rev().zip(signature.params.iter().rev()) {
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

    fn add(&mut self, code: &str) {
        self.code.push_str(code);
    }

    fn tabs(&mut self) {
        for _ in 0..self.depth {
            self.code.push_str("  ");
        }
    }

    fn escape(&mut self, name: &str) {
        for c in name.chars() {
            self.add(&format!("_{:x}", c as i32));
        }
    }

    fn make_declaration(&mut self, name: &str, gid: usize, cid: usize, signature: &Signature) {
        self.add("void hopf");
        self.escape(name);
        self.add(&format!("_{gid}_{cid}("));
        for (i, param) in signature.params.iter().enumerate() {
            if i > 0 {
                self.add(", ");
            }
            self.make_type(param);
        }
        for (i, ret) in signature.returns.iter().enumerate() {
            if i > 0 || !signature.params.is_empty() {
                self.add(", ");
            }
            self.make_type(&ret.inc());
        }
        self.add(");\n");
    }

    fn make_type(&mut self, ty: &Type) {
        match ty {
            Type::Custom(name, depth) => {
                self.add("hopt");
                self.escape(name);
                self.add(&"*".repeat(*depth));
            }
            Type::Byte(depth) => {
                self.add("uint8_t");
                self.add(&"*".repeat(*depth));
            }
            Type::Int(depth) => {
                self.add("int64_t");
                self.add(&"*".repeat(*depth));
            }
            Type::Float(depth) => {
                self.add("double");
                self.add(&"*".repeat(*depth));
            }
            Type::Bool(depth) => {
                self.add("uint8_t");
                self.add(&"*".repeat(*depth));
            }
        }
    }

    fn make_function(
        &mut self,
        name: &str,
        gid: usize,
        cid: usize,
        signature: &Signature,
        kind: &Kind,
    ) {
        match kind {
            Kind::Builtin => self.make_builtin(name, gid, cid, signature),
            Kind::Auto => self.make_auto(name, gid, cid, signature),
            Kind::Custom(body) => self.make_custom(name, gid, cid, signature, body),
        }
    }

    fn being_fn(&mut self, name: &str, gid: usize, cid: usize, signature: &Signature) {
        self.counter = signature.params.len();
        self.stack = signature.params.iter().cloned().enumerate().collect();

        self.add("void hopf");
        self.escape(name);
        self.add(&format!("_{gid}_{cid}("));
        for (i, param) in signature.params.iter().enumerate() {
            if i > 0 {
                self.add(", ");
            }
            self.make_type(param);
            self.add(&format!(" hopv_{i}"))
        }
        for (i, ret) in signature.returns.iter().enumerate() {
            if i > 0 || !signature.params.is_empty() {
                self.add(", ");
            }
            self.make_type(&ret.inc());
            self.add(&format!(" hopr_{i}"))
        }
        self.add(") {\n");
    }

    fn end_fn(&mut self) {
        for (i, (ret, _)) in self.stack.clone().iter().enumerate() {
            self.tabs();
            self.add(&format!("*hopr_{} = hopv_{};\n", i, ret));
        }
        self.add("}\n");
    }

    fn make_custom(
        &mut self,
        name: &str,
        gid: usize,
        cid: usize,
        signature: &Signature,
        body: &[Stmt],
    ) {
        self.being_fn(name, gid, cid, signature);
        for stmt in body {
            self.make_stmt(stmt);
        }
        self.end_fn();
    }

    fn make_auto(&mut self, name: &str, gid: usize, cid: usize, signature: &Signature) {
        todo!()
    }

    fn make_builtin(&mut self, name: &str, gid: usize, cid: usize, signature: &Signature) {
        self.being_fn(name, gid, cid, signature);
        match name {
            "+" => self.add("  *hopr_0 = hopv_0 + hopv_1;\n}\n"),
            "-" => self.add("  *hopr_0 = hopv_0 - hopv_1;\n}\n"),
            "*" => {
                if signature.params.len() == 1 {
                    self.add("  *hopr_0 = hopv_0 * hopv_1;\n}\n");
                } else {
                    self.add("  *hopr_0 = *hopv_0;\n}\n");
                }
            }
            "/" => self.add("  *hopr_0 = hopv_0 / hopv_1;\n}\n"),
            "%" => self.add("  *hopr_0 = hopv_0 % hopv_1;\n}\n"),
            "&" => {
                if signature.params[0] == Type::Bool(0) {
                    self.add("  *hopr_0 = hopv_0 && hopv_1;\n}\n");
                } else {
                    self.add("  *hopr_0 = hopv_0 & hopv_1;\n}\n");
                }
            }
            "|" => {
                if signature.params[0] == Type::Bool(0) {
                    self.add("  *hopr_0 = hopv_0 || hopv_1;\n}\n");
                } else {
                    self.add("  *hopr_0 = hopv_0 | hopv_1;\n}\n");
                }
            }
            "^" => self.add("  *hopr_0 = hopv_0 ^ hopv_1;\n}\n"),
            "!" => {
                if signature.params[0] == Type::Bool(0) {
                    self.add("  *hopr_0 = !hopv_0;\n}\n");
                } else {
                    self.add("  *hopr_0 = ~hopv_0;\n}\n");
                }
            }
            "neg" => self.add("  *hopr_0 = -hopv_0;\n}\n"),
            "size_of" => {
                self.add("  *hopr_0 = sizeof(");
                self.make_type(&signature.params[0]);
                self.add(")\n}\n");
            }
            "~" => self.add("}\n"),
            "==" => self.add("  *hopr_0 = hopv_0 == hopv_1;\n}\n"),
            "!=" => self.add("  *hopr_0 = hopv_0 != hopv_1;\n}\n"),
            "<" => self.add("  *hopr_0 = hopv_0 < hopv_1;\n}\n"),
            "<=" => self.add("  *hopr_0 = hopv_0 <= hopv_1;\n}\n"),
            ">" => self.add("  *hopr_0 = hopv_0 > hopv_1;\n}\n"),
            ">=" => self.add("  *hopr_0 = hopv_0 >= hopv_1;\n}\n"),
            "_" => self.add("}\n"),
            "." => self.add("  *hopr_0 = hopv_0;\n}\n"),
            "to_byte" => self.add("  *hopr_0 = (uint8_t) hopv_0;\n}\n"),
            "to_int" => self.add("  *hopr_0 = (int64_t) hopv_0;\n}\n"),
            "to_float" => self.add("  *hopr_0 = (double) hopv_0;\n}\n"),
            "to_bool_ptr" => self.add("  *hopr_0 = (uint8_t*) hopv_0;\n}\n"),
            "to_byte_ptr" => self.add("  *hopr_0 = (uint8_t*) hopv_0;\n}\n"),
            "to_int_ptr" => self.add("  *hopr_0 = (int64_t*) hopv_0;\n}\n"),
            "to_float_ptr" => self.add("  *hopr_0 = (double*) hopv_0;\n}\n"),
            "put" => match &signature.params[0] {
                Type::Byte(0) => self.add("  printf(\"%\" PRIu8, hopv_0);\n}\n"),
                Type::Int(0) => self.add("  printf(\"%\" PRId64, hopv_0);\n}\n"),
                Type::Float(0) => self.add("  printf(\"%d\", hopv_0);\n}\n"),
                Type::Bool(0) => {
                    self.add("  if (hopv_0) {printf(\"true\");} else {printf(\"false\");}\n}\n")
                }
                _ => unreachable!(),
            },
            "putln" => match &signature.params[0] {
                Type::Byte(0) => self.add("  printf(\"%\" PRIu8 \"\\n\", hopv_0);\n}\n"),
                Type::Int(0) => self.add("  printf(\"%\" PRId64 \"\\n\", hopv_0);\n}\n"),
                Type::Float(0) => self.add("  printf(\"%d\\n\", hopv_0);\n}\n"),
                Type::Bool(0) => self
                    .add("  if (hopv_0) {printf(\"true\\n\");} else {printf(\"false\\n\");}\n}\n"),
                _ => unreachable!(),
            },
            "ln" => self.add("  printf(\"\\n\");\n}"),
            "puts" => self.add("  printf(\"%s\", (char*) hopv_0);\n}\n"),
            "putlns" => self.add("  printf(\"%s\\n\", (char*) hopv_0);\n}\n"),
            "putc" => self.add("  printf(\"%lc\", (wint_t) hopv_0);\n}\n"),
            "putlnc" => self.add("  printf(\"%lc\\n\", (wint_t) hopv_0);\n}\n"),
            _ => todo!(),
        }
    }

    fn make_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Group(ops, _) => {
                for op in ops {
                    self.make_op(op);
                }
            }
            Stmt::If {
                test,
                body,
                else_part,
                ..
            } => self.make_if(test, body, else_part),
            Stmt::While { test, body, .. } => self.make_while(test, body),
            Stmt::For {
                low, high, body, ..
            } => self.make_for(low, high, body),
            Stmt::Let { names, body, .. } => self.make_let(names, body),
        }
    }

    fn make_op(&mut self, op: &Op) {
        match op {
            Op::Int(i, _) => {
                let var = self.push(Type::Int(0));
                self.tabs();
                self.add(&format!("int64_t hopv_{var} = {i};\n"));
            }
            Op::Float(f, _) => {
                let var = self.push(Type::Float(0));
                self.tabs();
                self.add(&format!("double hopv_{var} = {f:?};\n"));
            }
            Op::Bool(b, _) => {
                let var = self.push(Type::Bool(0));
                self.tabs();
                self.add(&format!("uint8_t hopv_{var} = {};\n", *b as u8));
            }
            Op::Char(c, _) => {
                let var = self.push(Type::Int(0));
                self.tabs();
                self.add(&format!("int64_t hopv_{var} = {};\n", *c as u64));
            }
            Op::String(s, _) => {
                let var = self.push(Type::Byte(1));
                self.tabs();
                self.add(&format!("uint8_t* hopv_{var} = (uint8_t*) {s:?};\n"));
            }
            Op::Name(name) => self.make_name(name.name),
            Op::Expr(expr, _) => self.make_expr(expr),
        }
    }

    fn make_name(&mut self, name: &str) {
        for binds in self.let_binds.iter().rev() {
            if let Some((var, ty)) = binds.get(name) {
                self.stack.push((*var, *ty));
                return;
            }
        }

        if name == "@" {
            let (last, ty) = *self.stack.last().unwrap();
            let var = self.push(ty.inc());
            self.tabs();
            self.make_type(&ty);
            self.add(&format!("* hopv_{var} = &hopv_{last}"));
        } else {
            for (gid, signature) in self.info.signatures.get(name).unwrap().iter().enumerate() {
                if let Some(signature) = self.get_signature(signature) {
                    for (cid, csig) in self
                        .concrete
                        .get(name)
                        .unwrap()
                        .clone()
                        .get(&gid)
                        .unwrap()
                        .iter()
                        .enumerate()
                    {
                        if csig == &signature {
                            self.make_call(name, gid, cid, &signature);
                            return;
                        }
                    }
                }
            }
        }
    }

    fn make_call(&mut self, name: &str, gid: usize, cid: usize, signature: &Signature) {
        let mut ret_vars = Vec::new();
        for ret in &signature.returns {
            self.tabs();
            self.make_type(ret);
            self.add(&format!(" hopv_{};\n", self.counter));
            ret_vars.push(self.counter);
            self.counter += 1;
        }
        self.tabs();
        self.add("hopf");
        self.escape(name);
        self.add(&format!("_{gid}_{cid}("));
        let mut args = Vec::new();
        for _ in 0..signature.params.len() {
            args.push(self.stack.pop().unwrap());
        }
        for (i, (var, _)) in args.iter().rev().enumerate() {
            if i > 0 {
                self.add(", ");
            }
            self.add(&format!("hopv_{var}"));
        }
        for (i, ret) in ret_vars.iter().enumerate() {
            if i > 0 || !args.is_empty() {
                self.add(", ");
            }
            self.add(&format!("&hopv_{ret}"));
        }
        self.add(");\n");
        for (ret, ty) in ret_vars.into_iter().zip(&signature.returns) {
            self.stack.push((ret, *ty));
        }
    }

    fn make_expr(&mut self, expr: &Expr) {
        let mut ops = Vec::new();
        flatten_expr(expr, &mut ops);
        for op in ops {
            self.make_op(&op);
        }
    }

    fn make_if(&mut self, test: &[Op], body: &[Stmt], else_part: &Option<ElsePart>) {
        if let Some(ElsePart {
            body: else_body, ..
        }) = else_part
        {
            self.make_if_else(test, body, else_body);
        } else {
            for op in test {
                self.make_op(op);
            }
            self.tabs();
            let (var, _) = self.stack.pop().unwrap();
            self.add(&format!("if (hopv_{var}) {{\n"));
            self.depth += 1;
            for stmt in body {
                self.make_stmt(stmt);
            }
            self.depth -= 1;
            self.tabs();
            self.add("}\n");
        }
    }

    fn make_if_else(&mut self, test: &[Op], body: &[Stmt], else_body: &[Stmt]) {
        let code_before = self.code.clone();
        let stack_before = self.stack.clone();

        for op in test {
            self.make_op(op);
        }
        self.stack.pop();
        for stmt in body {
            self.make_stmt(stmt);
        }

        self.code = code_before;

        let mut alloc = Vec::new();
        for (_, ty) in &self.stack.clone() {
            alloc.push((self.var(), *ty));
        }
        for (var, ty) in &alloc {
            self.tabs();
            self.make_type(ty);
            self.add(&format!(" hopv_{var};\n"));
        }

        self.stack = stack_before;

        for op in test {
            self.make_op(op);
        }
        self.tabs();
        let (var, _) = self.stack.pop().unwrap();
        self.add(&format!("if (hopv_{var}) {{\n"));
        self.depth += 1;
        for stmt in body {
            self.make_stmt(stmt);
        }
        for (var, _) in alloc.iter().rev() {
            self.tabs();
            let s = self.stack.pop().unwrap().0;
            self.add(&format!("hopv_{var} = hopv_{s};\n",));
        }
        self.depth -= 1;
        self.tabs();
        self.add("} else {\n");
        self.depth += 1;
        for stmt in else_body {
            self.make_stmt(stmt);
        }
        for (var, _) in alloc.iter().rev() {
            self.tabs();
            let s = self.stack.pop().unwrap().0;
            self.add(&format!("hopv_{var} = hopv_{s};\n",));
        }
        self.depth -= 1;
        self.tabs();
        self.add("}\n");
        for (var, ty) in alloc {
            self.stack.push((var, ty));
        }
    }

    fn make_while(&mut self, test: &[Op], body: &[Stmt]) {
        for op in test {
            self.make_op(op);
        }
        let test_var = self.var();
        let (test_bool, _) = self.stack.pop().unwrap();
        self.tabs();
        self.make_type(&Type::Bool(0));
        self.add(&format!(" hopv_{test_var} = hopv_{test_bool};\n"));
        self.tabs();
        self.add(&format!("while (hopv_{test_var}) {{\n"));
        self.depth += 1;
        for stmt in body {
            self.make_stmt(stmt);
        }
        for op in test {
            self.make_op(op);
        }
        let (test_bool, _) = self.stack.pop().unwrap();
        self.tabs();
        self.make_type(&Type::Bool(0));
        self.add(&format!(" hopv_{test_var} = hopv_{test_bool};\n"));
        self.depth -= 1;
        self.tabs();
        self.add("}\n");
    }

    fn make_for(&mut self, low: &[Op], high: &[Op], body: &[Stmt]) {
        for op in low {
            self.make_op(op);
        }
        let (low, ty) = self.stack.pop().unwrap();
        for op in high {
            self.make_op(op);
        }
        let (high, _) = self.stack.pop().unwrap();
        self.tabs();
        self.add("for (");
        self.make_type(&ty);
        let for_var = self.var();
        self.add(&format!(
            " hopv_{for_var} = hopv_{low}; hopv_{for_var} < hopv_{high}; hopv_{for_var}++) {{\n"
        ));
        self.stack.push((for_var, ty));
        self.depth += 1;
        for stmt in body {
            self.make_stmt(stmt);
        }
        self.depth -= 1;
        self.tabs();
        self.add("}\n");
    }

    fn make_let(&mut self, names: &[Name], body: &[Stmt]) {
        let mut binds = HashMap::new();
        for name in names.iter().rev() {
            binds.insert(name.name, self.stack.pop().unwrap());
        }
        self.let_binds.push(binds);
        for stmt in body {
            self.make_stmt(stmt);
        }
        self.let_binds.pop();
    }

    fn fill_graph(&mut self, name: &'static str, id: usize, binds: Vec<Type>) {
        let tup = (name, id, binds);
        if self.done.contains(&tup) {
            return;
        }
        let (_, _, binds) = tup;
        self.done.insert((name, id, binds.clone()));

        let signature = Signature::convert(&self.info.signatures.get(name).unwrap()[id], &binds);

        match self.concrete.entry(name) {
            hash_map::Entry::Vacant(v) => {
                v.insert(HashMap::from([(id, vec![signature])]));
            }
            hash_map::Entry::Occupied(mut o) => match o.get_mut().entry(id) {
                hash_map::Entry::Vacant(v) => {
                    v.insert(vec![signature]);
                }
                hash_map::Entry::Occupied(mut o) => o.get_mut().push(signature),
            },
        }

        for (call_name, call_id, call_binds) in
            self.info.graph.get(&(name, id)).into_iter().flatten()
        {
            let converted_binds: Vec<_> = call_binds
                .iter()
                .map(|ty| Type::convert(ty, &binds))
                .collect();
            self.fill_graph(call_name, *call_id, converted_binds);
        }
    }
}

fn flatten_expr(expr: &Expr, ops: &mut Vec<Op>) {
    match expr {
        Expr::Int(i, span) => ops.push(Op::Int(*i, *span)),
        Expr::Float(f, span) => ops.push(Op::Float(*f, *span)),
        Expr::Bool(b, span) => ops.push(Op::Bool(*b, *span)),
        Expr::Char(c, span) => ops.push(Op::Char(*c, *span)),
        Expr::Name(name) => ops.push(Op::Name(*name)),
        Expr::Add(left, right, _) => {
            flatten_expr(left, ops);
            flatten_expr(right, ops);
            ops.push(Op::Name(Name::new("+")));
        }
        Expr::And(left, right, _) => {
            flatten_expr(left, ops);
            flatten_expr(right, ops);
            ops.push(Op::Name(Name::new("&")));
        }
        Expr::Divide(left, right, _) => {
            flatten_expr(left, ops);
            flatten_expr(right, ops);
            ops.push(Op::Name(Name::new("/")));
        }
        Expr::Equal(left, right, _) => {
            flatten_expr(left, ops);
            flatten_expr(right, ops);
            ops.push(Op::Name(Name::new("==")));
        }
        Expr::GreaterEqual(left, right, _) => {
            flatten_expr(left, ops);
            flatten_expr(right, ops);
            ops.push(Op::Name(Name::new(">=")));
        }
        Expr::GreaterThan(left, right, _) => {
            flatten_expr(left, ops);
            flatten_expr(right, ops);
            ops.push(Op::Name(Name::new(">")));
        }
        Expr::LessEqual(left, right, _) => {
            flatten_expr(left, ops);
            flatten_expr(right, ops);
            ops.push(Op::Name(Name::new("<=")));
        }
        Expr::LessThan(left, right, _) => {
            flatten_expr(left, ops);
            flatten_expr(right, ops);
            ops.push(Op::Name(Name::new("<")));
        }
        Expr::Modulo(left, right, _) => {
            flatten_expr(left, ops);
            flatten_expr(right, ops);
            ops.push(Op::Name(Name::new("%")));
        }
        Expr::Multiply(left, right, _) => {
            flatten_expr(left, ops);
            flatten_expr(right, ops);
            ops.push(Op::Name(Name::new("*")));
        }
        Expr::NotEqual(left, right, _) => {
            flatten_expr(left, ops);
            flatten_expr(right, ops);
            ops.push(Op::Name(Name::new("!=")));
        }
        Expr::Or(left, right, _) => {
            flatten_expr(left, ops);
            flatten_expr(right, ops);
            ops.push(Op::Name(Name::new("|")));
        }
        Expr::Subtract(left, right, _) => {
            flatten_expr(left, ops);
            flatten_expr(right, ops);
            ops.push(Op::Name(Name::new("-")));
        }
        Expr::Xor(left, right, _) => {
            flatten_expr(left, ops);
            flatten_expr(right, ops);
            ops.push(Op::Name(Name::new("!")));
        }
        Expr::Not(expr, _) => {
            flatten_expr(expr, ops);
            ops.push(Op::Name(Name::new("!")));
        }
        Expr::Negate(expr, _) => {
            flatten_expr(expr, ops);
            ops.push(Op::Name(Name::new("neg")));
        }
        Expr::Group(group, _) => ops.extend(group.iter().cloned()),
    }
}

#[derive(Clone, PartialEq, Eq)]
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
    fn convert(signature: &GSignature, binds: &[Type]) -> Signature {
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

    fn convert(ty: &GType, binds: &[Type]) -> Type {
        match ty {
            GType::Primitive(prim) => match prim {
                Primitive::Int => Type::Int(0),
                Primitive::Float => Type::Float(0),
                Primitive::Byte => Type::Byte(0),
                Primitive::Bool => Type::Bool(0),
            },
            GType::Pointer(ty) => Type::convert(&*ty, binds).inc(),
            GType::Custom(name) => Type::Custom(name.name, 0),
            GType::Generic(index) => binds[*index],
        }
    }
}

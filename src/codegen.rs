use crate::ast::{ElsePart, Expr, Name, Op, Stmt};
use crate::typecheck::{flatten_expr, GSignature, GType, Kind, Primitive, TypeInfo};

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
#include <stdlib.h>
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

        for (name, fields) in &self.info.struct_fields {
            self.add("struct hopt");
            self.escape(name);
            self.add(" {\n");
            for (ty, field) in fields {
                self.add("    ");
                self.make_type(&Type::convert(ty, &[]));
                self.add(" hf");
                self.escape(field);
                self.add(";\n")
            }
            self.add("};\n");
        }

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
        self.add("    setlocale(LC_ALL, \"\");\n");
        self.add("    int64_t code = 0;\n");
        self.add("    hf_6d_61_69_6e_0_0(");
        if !main_signature.params.is_empty() {
            self.add("argc, (uint8_t**) argv");
        }
        if !main_signature.returns.is_empty() {
            if !main_signature.params.is_empty() {
                self.add(", ");
            }
            self.add("&code");
        }
        self.add(");\n    return code;\n}\n");
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
            self.code.push_str("    ");
        }
    }

    fn escape(&mut self, name: &str) {
        for c in name.chars() {
            self.add(&format!("_{:x}", c as i32));
        }
    }

    fn make_declaration(&mut self, name: &str, gid: usize, cid: usize, signature: &Signature) {
        self.add("void hf");
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
                self.add("struct hopt");
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
            Kind::Auto(_) => self.make_auto(name, gid, cid, signature),
            Kind::Custom(body) => self.make_custom(name, gid, cid, signature, body),
        }
    }

    fn being_fn(&mut self, name: &str, gid: usize, cid: usize, signature: &Signature) {
        self.counter = signature.params.len();
        self.stack = signature.params.iter().cloned().enumerate().collect();

        self.add("void hf");
        self.escape(name);
        self.add(&format!("_{gid}_{cid}("));
        for (i, param) in signature.params.iter().enumerate() {
            if i > 0 {
                self.add(", ");
            }
            self.make_type(param);
            self.add(&format!(" hv{i}"))
        }
        for (i, ret) in signature.returns.iter().enumerate() {
            if i > 0 || !signature.params.is_empty() {
                self.add(", ");
            }
            self.make_type(&ret.inc());
            self.add(&format!(" hr{i}"))
        }
        self.add(") {\n");
    }

    fn end_fn(&mut self) {
        for (i, (ret, _)) in self.stack.clone().iter().enumerate() {
            self.tabs();
            self.add(&format!("*hr{} = hv{};\n", i, ret));
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
        self.being_fn(name, gid, cid, signature);
        if name == "size_of" {
            self.add("    *hr0 = sizeof(");
            self.make_type(&signature.params[0]);
            self.add(");\n}\n");
        } else if let Some(field) = name.strip_prefix("..") {
            self.add("    *hr0 = hv0;\n");
            self.add("    *hr1 = hv0.hf");
            self.escape(field);
            self.add(";\n}\n");
        } else if let Some(field) = name.strip_prefix('.') {
            self.add("    *hr0 = hv0.hf");
            self.escape(field);
            self.add(";\n}\n");
        } else if name.starts_with("to_") && name.ends_with("_ptr") {
            self.add("    *hr0 = (");
            self.make_type(&signature.returns[0]);
            self.add(") hv0;\n}\n")
        } else {
            for (i, (_, field)) in self
                .info
                .struct_fields
                .get(name)
                .unwrap()
                .iter()
                .enumerate()
            {
                self.add("    hr0->hf");
                self.escape(field);
                self.add(&format!(" = hv{i};\n"));
            }
            self.add("}\n");
        }
    }

    fn make_builtin(&mut self, name: &str, gid: usize, cid: usize, signature: &Signature) {
        self.being_fn(name, gid, cid, signature);
        match name {
            "+" => self.add("    *hr0 = hv0 + hv1;\n}\n"),
            "-" => self.add("    *hr0 = hv0 - hv1;\n}\n"),
            "*" => {
                if signature.params.len() == 1 {
                    self.add("    *hr0 = *hv0;\n}\n");
                } else {
                    self.add("    *hr0 = hv0 * hv1;\n}\n");
                }
            }
            "/" => self.add("    *hr0 = hv0 / hv1;\n}\n"),
            "%" => self.add("    *hr0 = hv0 % hv1;\n}\n"),
            "&" => {
                if signature.params[0] == Type::Bool(0) {
                    self.add("    *hr0 = hv0 && hv1;\n}\n");
                } else {
                    self.add("    *hr0 = hv0 & hv1;\n}\n");
                }
            }
            "|" => {
                if signature.params[0] == Type::Bool(0) {
                    self.add("    *hr0 = hv0 || hv1;\n}\n");
                } else {
                    self.add("    *hr0 = hv0 | hv1;\n}\n");
                }
            }
            "^" => self.add("    *hr0 = hv0 ^ hv1;\n}\n"),
            "!" => {
                if signature.params[0] == Type::Bool(0) {
                    self.add("    *hr0 = !hv0;\n}\n");
                } else {
                    self.add("    *hr0 = ~hv0;\n}\n");
                }
            }
            "neg" => self.add("    *hr0 = -hv0;\n}\n"),
            "size_of" => {
                self.add("    *hr0 = sizeof(");
                self.make_type(&signature.params[0]);
                self.add(");\n}\n");
            }
            "~" => self.add("}\n"),
            "==" => self.add("    *hr0 = hv0 == hv1;\n}\n"),
            "!=" => self.add("    *hr0 = hv0 != hv1;\n}\n"),
            "<" => self.add("    *hr0 = hv0 < hv1;\n}\n"),
            "<=" => self.add("    *hr0 = hv0 <= hv1;\n}\n"),
            ">" => self.add("    *hr0 = hv0 > hv1;\n}\n"),
            ">=" => self.add("    *hr0 = hv0 >= hv1;\n}\n"),
            "_" => self.add("}\n"),
            "." => self.add("    *hr0 = hv0;\n    *hr1 = hv0;\n}\n"),
            "to_byte" => self.add("    *hr0 = (uint8_t) hv0;\n}\n"),
            "to_int" => self.add("    *hr0 = (int64_t) hv0;\n}\n"),
            "to_float" => self.add("    *hr0 = (double) hv0;\n}\n"),
            "to_bool_ptr" => self.add("    *hr0 = (uint8_t*) hv0;\n}\n"),
            "to_byte_ptr" => self.add("    *hr0 = (uint8_t*) hv0;\n}\n"),
            "to_int_ptr" => self.add("    *hr0 = (int64_t*) hv0;\n}\n"),
            "to_float_ptr" => self.add("    *hr0 = (double*) hv0;\n}\n"),
            "put" => match &signature.params[0] {
                Type::Byte(0) => self.add("    printf(\"%\" PRIu8, hv0);\n}\n"),
                Type::Int(0) => self.add("    printf(\"%\" PRId64, hv0);\n}\n"),
                Type::Float(0) => self.add("    printf(\"%.15g\", hv0);\n}\n"),
                Type::Bool(0) => {
                    self.add("    if (hv0) {printf(\"true\");} else {printf(\"false\");}\n}\n")
                }
                _ => self.add("    printf(\"%p\" , hv0);\n}\n"),
            },
            "putln" => match &signature.params[0] {
                Type::Byte(0) => self.add("    printf(\"%\" PRIu8 \"\\n\", hv0);\n}\n"),
                Type::Int(0) => self.add("    printf(\"%\" PRId64 \"\\n\", hv0);\n}\n"),
                Type::Float(0) => self.add("    printf(\"%.15g\\n\", hv0);\n}\n"),
                Type::Bool(0) => self
                    .add("    if (hv0) {printf(\"true\\n\");} else {printf(\"false\\n\");}\n}\n"),
                _ => self.add("    printf(\"%p\n\" , hv0);\n}\n"),
            },
            "ln" => self.add("    printf(\"\\n\");\n}"),
            "puts" => self.add("    printf(\"%s\", (char*) hv0);\n}\n"),
            "putlns" => self.add("    printf(\"%s\\n\", (char*) hv0);\n}\n"),
            "putc" => self.add("    printf(\"%lc\", (wint_t) hv0);\n}\n"),
            "putlnc" => self.add("    printf(\"%lc\\n\", (wint_t) hv0);\n}\n"),
            "store" => self.add("    *hv0 = hv1;\n}\n"),
            "panic" => self.add("}\n"),
            "assert" => self.add("}\n"),
            "@" => self.add("}\n"),
            _ => unreachable!(),
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
                self.add(&format!("int64_t hv{var} = {i};\n"));
            }
            Op::Float(f, _) => {
                let var = self.push(Type::Float(0));
                self.tabs();
                self.add(&format!("double hv{var} = {f:?};\n"));
            }
            Op::Bool(b, _) => {
                let var = self.push(Type::Bool(0));
                self.tabs();
                self.add(&format!("uint8_t hv{var} = {};\n", *b as u8));
            }
            Op::Char(c, _) => {
                let var = self.push(Type::Int(0));
                self.tabs();
                self.add(&format!("int64_t hv{var} = {};\n", *c as u64));
            }
            Op::String(s, _) => {
                let var = self.push(Type::Byte(1));
                self.tabs();
                self.add(&format!("uint8_t* hv{var} = (uint8_t*) {};\n", escape(s)));
            }
            Op::Name(name) => self.make_name(name),
            Op::Expr(expr, _) => self.make_expr(expr),
        }
    }

    fn make_name(&mut self, name: &Name) {
        let Name { name, span } = name;
        for binds in self.let_binds.iter().rev() {
            if let Some((var, ty)) = binds.get(name) {
                self.stack.push((*var, *ty));
                return;
            }
        }

        if *name == "@" {
            let (last, ty) = *self.stack.last().unwrap();
            let var = self.push(ty.inc());
            self.tabs();
            self.make_type(&ty);
            self.add(&format!("* hv{var} = &hv{last};\n"));
        } else if *name == "panic" {
            self.tabs();
            self.add(&format!(
                "printf({:?});\n",
                format!(
                    "panicked at {}:{}:{}\n",
                    span.file,
                    span.line + 1,
                    span.column + 1
                )
            ));
            self.tabs();
            self.add("exit(EXIT_FAILURE);\n");
        } else if *name == "assert" && matches!(self.stack.last(), Some((_, Type::Bool(0)))) {
            let (test, _) = self.stack.pop().unwrap();
            self.tabs();
            self.add(&format!("if (!hv{test}) {{\n"));
            self.tabs();
            self.add(&format!(
                "    printf({:?});\n",
                format!(
                    "assertion failed at {}:{}:{}\n",
                    span.file,
                    span.line + 1,
                    span.column + 1
                )
            ));
            self.tabs();
            self.add("    exit(EXIT_FAILURE);\n");
            self.tabs();
            self.add("}\n");
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
            self.add(&format!(" hv{};\n", self.counter));
            ret_vars.push(self.counter);
            self.counter += 1;
        }
        self.tabs();
        self.add("hf");
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
            self.add(&format!("hv{var}"));
        }
        for (i, ret) in ret_vars.iter().enumerate() {
            if i > 0 || !args.is_empty() {
                self.add(", ");
            }
            self.add(&format!("&hv{ret}"));
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
            let stack_before = self.stack.clone();
            self.add(&format!("if (hv{var}) {{\n"));
            self.depth += 1;
            for stmt in body {
                self.make_stmt(stmt);
            }
            for ((b, _), (s, _)) in stack_before.iter().zip(&self.stack.clone()) {
                self.tabs();
                self.add(&format!("hv{b} = hv{s};\n"));
            }
            self.stack = stack_before;
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
            self.add(&format!(" hv{var};\n"));
        }

        self.stack = stack_before;

        for op in test {
            self.make_op(op);
        }
        self.tabs();
        let (var, _) = self.stack.pop().unwrap();
        self.add(&format!("if (hv{var}) {{\n"));
        self.depth += 1;
        for stmt in body {
            self.make_stmt(stmt);
        }
        for (var, _) in alloc.iter().rev() {
            self.tabs();
            let s = self.stack.pop().unwrap().0;
            self.add(&format!("hv{var} = hv{s};\n",));
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
            self.add(&format!("hv{var} = hv{s};\n",));
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
        self.add(&format!(" hv{test_var} = hv{test_bool};\n"));
        self.tabs();
        self.add(&format!("while (hv{test_var}) {{\n"));
        self.depth += 1;
        for stmt in body {
            self.make_stmt(stmt);
        }
        for op in test {
            self.make_op(op);
        }
        let (test_bool, _) = self.stack.pop().unwrap();
        self.tabs();
        self.add(&format!("hv{test_var} = hv{test_bool};\n"));
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
            " hv{for_var} = hv{low}; hv{for_var} < hv{high}; hv{for_var}++) {{\n"
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

fn escape(s: &str) -> String {
    let mut out = "\"".to_string();
    for c in s.chars() {
        if (c as u32) < 256 {
            out.push_str(&format!("\\x{:01$x}", c as u32, 2));
        } else if (c as u32) < 65536 {
            out.push_str(&format!("\\u{:01$x}", c as u32, 4));
        } else {
            out.push_str(&format!("\\U{:01$x}", c as u32, 8));
        }
    }
    out.push('"');
    out
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
            GType::Pointer(ty) => Type::convert(ty, binds).inc(),
            GType::Custom(name) => Type::Custom(name.name, 0),
            GType::Generic(index) => binds[*index],
        }
    }
}

use std::collections::{hash_map, HashMap, HashSet};
use std::mem;

use crate::program::{Program, ProgramOp, ProgramStmt};
use crate::typecheck::{CallInfo, FnInfo, ProgramInfo};
use crate::types::{GSignature, GTypeId, Kind, TypeId, Types};

const PRELUDE: &str = "\
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h>
#include <wchar.h>
#include <locale.h>
#include <math.h>
#include <time.h>
#include <string.h>
";

struct Generator {
    program: Program,
    call_graph: HashMap<FnInfo, HashSet<CallInfo>>,
    signatures: HashMap<Vec<&'static str>, Vec<GSignature>>,
    struct_fields: HashMap<Vec<&'static str>, Vec<(&'static str, GTypeId)>>,
    types: Types,
    #[allow(clippy::type_complexity)]
    concrete: HashMap<
        (Vec<&'static str>, usize),
        HashMap<Vec<TypeId>, (usize, Kind, Vec<TypeId>, Vec<TypeId>)>,
    >,
    main_unit_prefix: Vec<&'static str>,
    code: String,
    depth: usize,
    counter: usize,
}

pub fn generate(info: ProgramInfo) -> String {
    let ProgramInfo {
        program,
        call_graph,
        signatures,
        struct_fields,
        types,
        main_unit_prefix,
    } = info;
    let mut context = Generator {
        program,
        call_graph,
        signatures,
        struct_fields,
        types,
        concrete: HashMap::new(),
        main_unit_prefix,
        code: PRELUDE.to_string(),
        depth: 0,
        counter: 0,
    };
    context.generate();
    context.code
}

impl Generator {
    fn generate(&mut self) {
        let mut done = HashSet::new();
        let mut main = self.main_unit_prefix.to_vec();
        main.push("main");
        self.get_concrete_signatures(&main, 0, Vec::new(), &mut done);

        for (name, map) in &self.types.custom_map {
            for id in map.values() {
                self.code
                    .push_str(&format!("struct hs_{}_{id};\n", escape_names(name)));
            }
        }

        let split_point = self.code.len();

        let mut structs = Vec::new();
        let cloned_map = self.types.custom_map.clone();
        for (name, fields) in &self.struct_fields {
            for (binds, id) in cloned_map.get(name).into_iter().flatten() {
                let mut code = format!("struct hs_{}_{id} {{\n", escape_names(name));
                let mut ftypes = Vec::new();
                for (fname, gtype) in fields {
                    let id = self.types.substitute_concrete(*gtype, binds);
                    ftypes.push(id);
                    code.push_str("    ");
                    code.push_str(&self.types.generate(id));
                    code.push_str(" hm_");
                    code.push_str(&escape_name(fname));
                    code.push_str(";\n");
                }
                code.push_str("};\n");
                structs.push((code, *id, ftypes));
            }
        }

        self.types.sort_structs(&mut structs);

        for (struct_code, _, _) in structs {
            self.code.push_str(&struct_code);
        }

        for global in &self.program.globals {
            self.code.push_str(&format!(
                "{} hg_{};\n",
                self.types.generate(global.type_id),
                escape_names(&global.name)
            ));
        }

        for ((name, _), map) in &self.concrete {
            for (params, (id, _, returns, _)) in map {
                self.code
                    .push_str(&self.get_declaration(name, *id, params, returns));
            }
        }

        let concrete = self.concrete.clone();
        for ((name, _), map) in &concrete {
            for (params, (id, kind, returns, _)) in map {
                match kind {
                    Kind::Builtin => self.generate_builtin(name[0], *id, params, returns),
                    Kind::Custom(_) => {}
                    Kind::Constructor(_) => self.generate_constructor(name, *id, params, returns),
                    Kind::Member(_) => self.generate_member(name, *id, params, returns),
                    Kind::Global(_) => self.generate_global(name, *id, params, returns),
                    Kind::GlobalWrite(_) => self.generate_global_write(name, *id, params, returns),
                    Kind::GlobalPtr(_) => self.generate_global_ptr(name, *id, params, returns),
                }
            }
        }

        let functions = mem::take(&mut self.program.functions);
        for function in functions {
            for (params, (id, _, returns, binds)) in concrete
                .get(&(function.name.clone(), function.index))
                .into_iter()
                .flatten()
            {
                self.generate_custom(
                    &function.name,
                    *id,
                    binds,
                    params,
                    returns,
                    &function.body,
                    &function.vars,
                    &function.return_vars,
                );
            }
        }

        self.generate_main();

        let fn_ptr_typedefs = self.types.generate_fn_ptr_typedefs();
        let mut new_code = self.code[..split_point].to_string();
        new_code.push_str(&fn_ptr_typedefs);
        new_code.push_str(&self.code[split_point..]);
        self.code = new_code;
    }

    fn generate_main(&mut self) {
        self.code.push_str(
            "int main(int argc, char** argv) {
    int64_t code = 0;
    srand(time(NULL));
    setlocale(LC_ALL, \"\");\n",
        );

        self.depth = 2;
        let globals = self.program.globals.clone();
        for global in globals {
            if let Some(init) = &global.init {
                self.code.push_str("    {\n");
                self.generate_vars(&init.vars, &[]);
                for op in &init.ops {
                    self.generate_op(op, &[]);
                }
                self.code.push_str(&format!(
                    "        hg_{} = hv{};\n",
                    escape_names(&global.name),
                    init.var
                ));
                self.code.push_str("    }\n");
            }
        }

        let mut main = self.main_unit_prefix.to_vec();
        main.push("main");
        let main_signature = &self.signatures[&main][0];

        self.code
            .push_str(&format!("    hf_{}_0(", escape_names(&main)));
        if !main_signature.params.is_empty() {
            self.code.push_str("argc, (uint8_t**) argv");
        }
        if !main_signature.returns.is_empty() {
            if main_signature.params.is_empty() {
                self.code.push_str("&code");
            } else {
                self.code.push_str(", &code");
            }
        }
        self.code.push_str(");\n    return (int) code;\n}\n");
    }

    fn generate_topline(
        &mut self,
        name: &[&str],
        id: usize,
        params: &[TypeId],
        returns: &[TypeId],
    ) {
        self.code.push_str("void hf_");
        self.code.push_str(&escape_names(name));
        self.code.push_str(&format!("_{}(", id));

        for (i, param) in params.iter().enumerate() {
            if i > 0 {
                self.code.push_str(", ");
            }
            self.code
                .push_str(&format!("{} hv{i}", self.types.generate(*param)));
        }

        for (i, ret) in returns.iter().enumerate() {
            if !params.is_empty() || i > 0 {
                self.code.push_str(", ");
            }
            self.code.push_str(&self.types.generate(*ret));
            self.code.push('*');
            self.code.push_str(&format!(" hr{i}"));
        }

        self.code.push_str(") {\n");
    }

    #[allow(clippy::too_many_arguments)]
    fn generate_custom(
        &mut self,
        name: &[&str],
        id: usize,
        binds: &[TypeId],
        params: &[TypeId],
        returns: &[TypeId],
        body: &[ProgramStmt],
        vars: &HashMap<GTypeId, Vec<usize>>,
        return_vars: &[usize],
    ) {
        self.generate_topline(name, id, params, returns);
        self.tab();

        self.generate_vars(vars, binds);

        for stmt in body {
            self.generate_stmt(stmt, binds);
        }

        for (i, var) in return_vars.iter().enumerate() {
            self.code.push_str(&format!("    *hr{i} = hv{var};\n"));
        }

        self.untab();
        self.code.push_str("}\n");
    }

    fn generate_vars(&mut self, vars: &HashMap<GTypeId, Vec<usize>>, binds: &[TypeId]) {
        for (id, vars) in vars {
            let id = self.types.substitute_concrete(*id, binds);
            let depth = self.types.concrete_depth(id);
            self.generate_tabs();
            self.code.push_str(&self.types.generate(id));
            self.code.push(' ');
            for (i, var) in vars.iter().enumerate() {
                if i == 0 {
                    self.code.push_str(&format!("hv{var}"));
                } else {
                    let stars = if self.types.is_fn_ptr_concrete(id) {
                        0
                    } else {
                        depth
                    };
                    self.code
                        .push_str(&format!(", {}hv{var}", "*".repeat(stars)));
                }
            }
            self.code.push_str(";\n");
        }
    }

    fn generate_stmt(&mut self, stmt: &ProgramStmt, binds: &[TypeId]) {
        match stmt {
            ProgramStmt::Op(op) => self.generate_op(op, binds),
            ProgramStmt::If {
                test_var,
                body,
                else_body,
                resolve,
            } => self.generate_if(*test_var, body, else_body, resolve, binds),
            ProgramStmt::For {
                iter_var,
                low_var,
                high_var,
                body,
                resolve,
            } => self.generate_for(*iter_var, *low_var, *high_var, body, resolve, binds),
            ProgramStmt::While {
                test_var,
                body,
                resolve,
            } => self.generate_while(*test_var, body, resolve, binds),
        }
    }

    fn generate_op(&mut self, op: &ProgramOp, binds: &[TypeId]) {
        self.generate_tabs();
        match op {
            ProgramOp::Int(var, val) => self.code.push_str(&format!("hv{var} = {val};\n")),
            ProgramOp::Float(var, val) => self.code.push_str(&format!("hv{var} = {val:?};\n")),
            ProgramOp::Bool(var, val) => {
                self.code.push_str(&format!("hv{var} = {};\n", *val as u8));
            }
            ProgramOp::Byte(var, val) => self.code.push_str(&format!("hv{var} = {val};\n")),
            ProgramOp::String(var, val) => self
                .code
                .push_str(&format!("hv{var} = (uint8_t*) {};\n", escape_string(val))),
            ProgramOp::Call(name, index, params, returns, call_binds) => {
                let id = self.get_id(name, *index, call_binds, binds);
                self.code
                    .push_str(&format!("hf_{}_{id}(", escape_names(name)));
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        self.code.push_str(", ");
                    }
                    self.code.push_str(&format!("hv{param}"));
                }
                for (i, ret) in returns.iter().enumerate() {
                    if !params.is_empty() || i > 0 {
                        self.code.push_str(", ");
                    }
                    self.code.push_str(&format!("&hv{ret}"));
                }
                self.code.push_str(");\n");
            }
            ProgramOp::Ref(var, old_var, _) => {
                self.code.push_str(&format!("hv{var} = &hv{old_var};\n"));
            }
            ProgramOp::Assert(var, span) => {
                let msg = format!("assertion failed at {}", span.location());
                self.code.push_str(&format!(
                    "if (!hv{var}) {{ printf(\"%s\\n\", {}); exit(1); }}\n",
                    escape_string(&msg)
                ));
            }
            ProgramOp::Abort(span) => {
                let msg = format!("panicked at {}", span.location());
                self.code.push_str(&format!(
                    "printf(\"%s\\n\", {}); exit(1);\n",
                    escape_string(&msg)
                ));
            }
            ProgramOp::SizeOf(var, id) => {
                let id = self.types.substitute_concrete(*id, binds);
                self.code
                    .push_str(&format!("hv{var} = sizeof({});\n", self.types.generate(id)));
            }
            ProgramOp::Alloc(var, id) => {
                let id = self.types.substitute_concrete(*id, binds);
                self.code.push_str(&format!(
                    "hv{var} = malloc(sizeof({}));\n",
                    self.types.generate(id)
                ));
            }
            ProgramOp::Zalloc(var, id) => {
                let id = self.types.substitute_concrete(*id, binds);
                self.code.push_str(&format!(
                    "hv{var} = calloc(1, sizeof({}));\n",
                    self.types.generate(id)
                ));
            }
            ProgramOp::AllocArr(var, len_var, id) => {
                let id = self.types.substitute_concrete(*id, binds);
                self.code.push_str(&format!(
                    "hv{var} = malloc(hv{len_var}*sizeof({}));\n",
                    self.types.generate(id)
                ));
            }
            ProgramOp::ZallocArr(var, len_var, id) => {
                let id = self.types.substitute_concrete(*id, binds);
                self.code.push_str(&format!(
                    "hv{var} = calloc(hv{len_var}, sizeof({}));\n",
                    self.types.generate(id)
                ));
            }
            ProgramOp::CastTo(var, old_var, id) => {
                let id = self.types.substitute_concrete(*id, binds);
                self.code.push_str(&format!(
                    "hv{var} = ({}) hv{old_var};\n",
                    self.types.generate(id)
                ));
            }
            ProgramOp::FnPtrCall(var, params, returns) => {
                self.code.push_str(&format!("(*hv{var})("));
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        self.code.push_str(", ");
                    }
                    self.code.push_str(&format!("hv{param}"));
                }
                for (i, ret) in returns.iter().enumerate() {
                    if i > 0 || !params.is_empty() {
                        self.code.push_str(", ");
                    }
                    self.code.push_str(&format!("&hv{ret}"));
                }
                self.code.push_str(");\n");
            }
            ProgramOp::FnRef(var, name, fn_binds) => {
                let id = self.get_id(name, 0, fn_binds, binds);
                self.code
                    .push_str(&format!("hv{var} = &hf_{}_{id};\n", escape_names(name)));
            }
        }
    }

    fn generate_if(
        &mut self,
        test_var: usize,
        body: &[ProgramStmt],
        else_body: &[ProgramStmt],
        resolve: &[(usize, usize, GTypeId)],
        binds: &[TypeId],
    ) {
        self.generate_tabs();
        self.code.push_str(&format!("if (hv{test_var}) {{\n"));
        self.tab();
        for stmt in body {
            self.generate_stmt(stmt, binds);
        }
        self.untab();
        self.generate_tabs();
        self.code.push_str("} else {\n");
        self.tab();
        for stmt in else_body {
            self.generate_stmt(stmt, binds);
        }
        self.generate_resolve(resolve, binds);
        self.untab();
        self.generate_tabs();
        self.code.push_str("}\n");
    }

    fn generate_for(
        &mut self,
        iter_var: usize,
        low_var: usize,
        high_var: usize,
        body: &[ProgramStmt],
        resolve: &[(usize, usize, GTypeId)],
        binds: &[TypeId],
    ) {
        self.generate_tabs();
        self.code.push_str(&format!(
            "for (hv{iter_var} = hv{low_var}; hv{iter_var} < hv{high_var}; hv{iter_var}++) {{\n"
        ));
        self.tab();
        for stmt in body {
            self.generate_stmt(stmt, binds);
        }
        self.generate_resolve(resolve, binds);
        self.untab();
        self.generate_tabs();
        self.code.push_str("}\n");
    }

    fn generate_while(
        &mut self,
        test_var: usize,
        body: &[ProgramStmt],
        resolve: &[(usize, usize, GTypeId)],
        binds: &[TypeId],
    ) {
        self.generate_tabs();
        self.code.push_str(&format!("while (hv{test_var}) {{\n"));
        self.tab();
        for stmt in body {
            self.generate_stmt(stmt, binds);
        }
        self.generate_resolve(resolve, binds);
        self.untab();
        self.generate_tabs();
        self.code.push_str("}\n");
    }

    fn generate_resolve(&mut self, resolve: &[(usize, usize, GTypeId)], binds: &[TypeId]) {
        if let [(new, old, _)] = resolve {
            self.generate_tabs();
            self.code.push_str(&format!("hv{new} = hv{old};\n"));
        } else if resolve.len() > 1 {
            self.generate_tabs();
            self.code.push_str("{\n");
            self.tab();
            for (i, (_, old, id)) in resolve.iter().enumerate() {
                self.generate_tabs();
                let id = self.types.substitute_concrete(*id, binds);
                self.code
                    .push_str(&format!("{} ht{i} = hv{old};\n", self.types.generate(id)));
            }
            for (i, (new, _, _)) in resolve.iter().enumerate() {
                self.generate_tabs();
                self.code.push_str(&format!("hv{new} = ht{i};\n"));
            }
            self.untab();
            self.generate_tabs();
            self.code.push_str("}\n");
        }
    }

    fn generate_builtin(&mut self, name: &str, id: usize, params: &[TypeId], returns: &[TypeId]) {
        self.generate_topline(&[name], id, params, returns);
        match name {
            "+" => self.code.push_str("    *hr0 = hv0 + hv1;\n"),
            "-" => self.code.push_str("    *hr0 = hv0 - hv1;\n"),
            "*" => self.code.push_str("    *hr0 = hv0 * hv1;\n"),
            "/" => self.code.push_str("    *hr0 = hv0 / hv1;\n"),
            "%" => self.code.push_str("    *hr0 = hv0 % hv1;\n"),
            "&" => self.code.push_str("    *hr0 = hv0 & hv1;\n"),
            "^" => self.code.push_str("    *hr0 = hv0 ^ hv1;\n"),
            "|" => self.code.push_str("    *hr0 = hv0 | hv1;\n"),
            "<<" => self.code.push_str("    *hr0 = hv0 << hv1;\n"),
            ">>" => self.code.push_str("    *hr0 = hv0 >> hv1;\n"),
            "==" => self.code.push_str("    *hr0 = hv0 == hv1;\n"),
            "!=" => self.code.push_str("    *hr0 = hv0 != hv1;\n"),
            "<" => self.code.push_str("    *hr0 = hv0 < hv1;\n"),
            "<=" => self.code.push_str("    *hr0 = hv0 <= hv1;\n"),
            ">" => self.code.push_str("    *hr0 = hv0 > hv1;\n"),
            ">=" => self.code.push_str("    *hr0 = hv0 >= hv1;\n"),
            "!" => self.code.push_str("    *hr0 = !hv0;\n"),
            "neg" => self.code.push_str("    *hr0 = -hv0;\n"),
            "." => self.code.push_str("    *hr0 = hv0; *hr1 = hv0;\n"),
            "~" => {}
            "@" => {}
            "to_byte" => self.code.push_str("    *hr0 = (uint8_t) hv0;\n"),
            "to_int" => self.code.push_str("    *hr0 = (int64_t) hv0;\n"),
            "to_float" => self.code.push_str("    *hr0 = (double) hv0;\n"),
            "put" => {
                if params[0] == self.types.concrete_int() {
                    self.code.push_str("    printf(\"%\" PRId64, hv0);\n");
                } else if params[0] == self.types.concrete_float() {
                    self.code.push_str("    printf(\"%.15g\", hv0);\n");
                } else if params[0] == self.types.concrete_byte() {
                    self.code.push_str("    printf(\"%\" PRIu8, hv0);\n");
                } else if params[0] == self.types.concrete_bool() {
                    self.code
                        .push_str("    if (hv0) {printf(\"true\");} else {printf(\"false\");}\n");
                }
            }
            "putln" => {
                if params[0] == self.types.concrete_int() {
                    self.code
                        .push_str("    printf(\"%\" PRId64 \"\\n\", hv0);\n");
                } else if params[0] == self.types.concrete_float() {
                    self.code.push_str("    printf(\"%.15g\\n\", hv0);\n");
                } else if params[0] == self.types.concrete_byte() {
                    self.code
                        .push_str("    printf(\"%\" PRIu8 \"\\n\", hv0);\n");
                } else if params[0] == self.types.concrete_bool() {
                    self.code.push_str(
                        "    if (hv0) {printf(\"true\\n\");} else {printf(\"false\\n\");}\n",
                    );
                }
            }
            "puts" => self.code.push_str("    printf(\"%s\", (char*) hv0);\n"),
            "putlns" => self.code.push_str("    printf(\"%s\\n\", (char*) hv0);\n"),
            "putc" => self.code.push_str("    printf(\"%lc\", (wint_t) hv0);\n"),
            "putlnc" => self
                .code
                .push_str("    printf(\"%lc\\n\", (wint_t) hv0);\n"),
            "putp" => self.code.push_str("    printf(\"%p\" , hv0);\n"),
            "putlnp" => self.code.push_str("    printf(\"%p\\n\" , hv0);\n"),
            "ln" => self.code.push_str("    printf(\"\\n\");\n"),
            "read" => self.code.push_str("    *hr0 = *hv0;\n"),
            "write" => self.code.push_str("    *hv0 = hv1;\n"),
            "exit" => self.code.push_str("    exit((int) hv0);\n"),
            "realloc" => {
                let id = self.types.deref_n_concrete(params[0], 1);
                self.code.push_str(&format!(
                    "    *hr0 = realloc(hv0, hv1*sizeof({}));\n",
                    self.types.generate(id)
                ));
            }
            "free" => self.code.push_str("    free(hv0);\n"),
            "copy" => {
                let id = self.types.deref_n_concrete(params[0], 1);
                self.code.push_str(&format!(
                    "    memmove(hv0, hv1, hv2*sizeof({}));\n",
                    self.types.generate(id)
                ));
            }
            "pow" => self.code.push_str("    *hr0 = pow(hv0, hv1);\n"),
            "random" => self
                .code
                .push_str("    *hr0 = rand()/((double) RAND_MAX + 1);\n"),
            "strcmp" => self
                .code
                .push_str("    *hr0 = strcmp((char*) hv0, (char*) hv1);\n"),
            "streq" => self
                .code
                .push_str("    *hr0 = strcmp((char*) hv0, (char*) hv1) == 0;\n"),
            "strcpy" => self
                .code
                .push_str("    strcpy((char*) hv0, (char*) hv1);\n"),
            "strlen" => self.code.push_str("    *hr0 = strlen((char*) hv0);\n"),
            "read_file" => self.code.push_str(
                "    char* buf = NULL;
    long length;
    FILE* f = fopen ((char*) hv0, \"r\");
    if (f) {
        fseek(f, 0, SEEK_END);
        length = ftell(f);
        fseek(f, 0, SEEK_SET);
        buf = malloc(length);
        if (buf) {
            fread(buf, 1, length, f);
        }
        fclose(f);
    }
    *hr0 = (uint8_t*) buf;\n",
            ),
            "write_to_file" => self.code.push_str(
                "   FILE *f = fopen((char*) hv1, \"w\");
    if (f) {
        if (fputs((char*) hv0, f) == EOF) {
            *hr0 = 0;
        }
        if (fclose(f) == EOF) {
            *hr0 = 0;
        }
        *hr0 = 1;
    } else {
        *hr0 = 0;
    }\n",
            ),
            "append_to_file" => self.code.push_str(
                "   FILE *f = fopen((char*) hv1, \"a\");
    if (f) {
        if (fputs((char*) hv0, f) == EOF) {
            *hr0 = 0;
            fclose(f);
        } else if (fclose(f) == EOF) {
            *hr0 = 0;
        } else {
            *hr0 = 1;
        }
    } else {
        *hr0 = 0;
    }\n",
            ),
            "time" => self.code.push_str(
                "    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    *hr0 = ts.tv_sec + ts.tv_nsec/1000000000.0;\n",
            ),
            "stdin" => self.code.push_str(
                "    char* buf = NULL;
    size_t n = 0;
    if (getline(&buf, &n, stdin) == -1) {
        free(buf);
        *hr0 = NULL;
    } else {
        *hr0 = (uint8_t*) buf;
    }\n",
            ),
            _ => unreachable!(),
        }
        self.code.push_str("}\n");
    }

    fn generate_constructor(
        &mut self,
        name: &[&str],
        id: usize,
        params: &[TypeId],
        returns: &[TypeId],
    ) {
        self.generate_topline(name, id, params, returns);
        for (i, field) in self.struct_fields[name].iter().enumerate() {
            self.code
                .push_str(&format!("    hr0->hm_{} = hv{i};\n", escape_name(field.0)));
        }
        self.code.push_str("}\n");
    }

    fn generate_member(&mut self, name: &[&str], id: usize, params: &[TypeId], returns: &[TypeId]) {
        self.generate_topline(name, id, params, returns);
        let (field_name, ddot) = match name.last().unwrap().strip_prefix("..") {
            Some(field_name) => (field_name, true),
            None => (name.last().unwrap().strip_prefix('.').unwrap(), false),
        };
        if self.types.concrete_depth(params[0]) == 0 {
            if ddot {
                self.code.push_str("    *hr0 = hv0;\n");
                self.code.push_str(&format!(
                    "    *hr1 = hv0.hm_{};\n}}\n",
                    escape_name(field_name)
                ));
            } else {
                self.code.push_str(&format!(
                    "    *hr0 = hv0.hm_{};\n}}\n",
                    escape_name(field_name)
                ));
            }
        } else if ddot {
            self.code.push_str("    *hr0 = hv0;\n");
            self.code.push_str(&format!(
                "    *hr1 = &(hv0->hm_{});\n}}\n",
                escape_name(field_name)
            ));
        } else {
            self.code.push_str(&format!(
                "    *hr0 = &(hv0->hm_{});\n}}\n",
                escape_name(field_name)
            ));
        }
    }

    fn generate_global(&mut self, name: &[&str], id: usize, params: &[TypeId], returns: &[TypeId]) {
        self.generate_topline(name, id, params, returns);
        self.code
            .push_str(&format!("    *hr0 = hg_{};\n}}\n", escape_names(name)));
    }

    fn generate_global_write(
        &mut self,
        name: &[&str],
        id: usize,
        params: &[TypeId],
        returns: &[TypeId],
    ) {
        let global_name = name.last().unwrap().strip_prefix("write_").unwrap();
        let mut qname = name[..name.len() - 1].to_vec();
        qname.push(global_name);
        self.generate_topline(name, id, params, returns);
        self.code
            .push_str(&format!("    hg_{} = hv0;\n}}\n", escape_names(&qname)));
    }

    fn generate_global_ptr(
        &mut self,
        name: &[&str],
        id: usize,
        params: &[TypeId],
        returns: &[TypeId],
    ) {
        let global_name = name.last().unwrap().strip_suffix("_ptr").unwrap();
        let mut qname = name[..name.len() - 1].to_vec();
        qname.push(global_name);
        self.generate_topline(name, id, params, returns);
        self.code
            .push_str(&format!("    *hr0 = &hg_{};\n}}\n", escape_names(&qname)));
    }

    fn get_declaration(
        &self,
        name: &[&str],
        id: usize,
        params: &[TypeId],
        returns: &[TypeId],
    ) -> String {
        let mut out = "void hf_".to_string();
        out.push_str(&escape_names(name));
        out.push_str(&format!("_{}(", id));

        for (i, param) in params.iter().enumerate() {
            if i > 0 {
                out.push_str(", ");
            }
            out.push_str(&self.types.generate(*param));
        }

        for (i, ret) in returns.iter().enumerate() {
            if !params.is_empty() || i > 0 {
                out.push_str(", ");
            }
            out.push_str(&self.types.generate(*ret));
            out.push('*');
        }

        out.push_str(");\n");
        out
    }

    fn get_id(
        &mut self,
        name: &[&str],
        index: usize,
        binds: &[GTypeId],
        fn_binds: &[TypeId],
    ) -> usize {
        let generic_params: Vec<_> = self.signatures[name][index]
            .params
            .iter()
            .map(|id| self.types.substitute(*id, binds))
            .collect();
        let params: Vec<_> = generic_params
            .into_iter()
            .map(|id| self.types.substitute_concrete(id, fn_binds))
            .collect();
        let vec = name.to_vec();
        self.concrete[&(vec, index)][&params].0
    }

    fn get_concrete_signatures(
        &mut self,
        name: &[&'static str],
        index: usize,
        binds: Vec<TypeId>,
        done: &mut HashSet<(Vec<&'static str>, usize, Vec<TypeId>)>,
    ) {
        let tup = (name.to_vec(), index, binds);
        if done.contains(&tup) {
            return;
        }
        let (name, _, binds) = tup;
        done.insert((name.clone(), index, binds.clone()));

        let signature = &self.signatures[&name][index];
        let params: Vec<_> = signature
            .params
            .iter()
            .map(|id| self.types.substitute_concrete(*id, &binds))
            .collect();
        let returns: Vec<_> = signature
            .returns
            .iter()
            .map(|id| self.types.substitute_concrete(*id, &binds))
            .collect();

        self.counter += 1;
        match self.concrete.entry((name.clone(), index)) {
            hash_map::Entry::Vacant(v) => {
                v.insert(HashMap::from([(
                    params,
                    (self.counter - 1, signature.kind, returns, binds.clone()),
                )]));
            }
            hash_map::Entry::Occupied(mut o) => {
                let map = o.get_mut();
                map.insert(
                    params,
                    (self.counter - 1, signature.kind, returns, binds.clone()),
                );
            }
        }

        for (call_name, call_index, call_binds) in self
            .call_graph
            .get(&(name, index))
            .cloned()
            .into_iter()
            .flatten()
        {
            let converted_binds: Vec<_> = call_binds
                .iter()
                .map(|id| self.types.substitute_concrete(*id, &binds))
                .collect();
            self.get_concrete_signatures(&call_name, call_index, converted_binds, done);
        }
    }

    fn tab(&mut self) {
        self.depth += 1;
    }

    fn untab(&mut self) {
        self.depth -= 1;
    }

    fn generate_tabs(&mut self) {
        self.code.push_str(&"    ".repeat(self.depth));
    }
}

pub fn escape_name(name: &str) -> String {
    let mut out = String::new();
    for c in name.chars() {
        if c.is_ascii_alphanumeric() {
            out.push(c);
        } else if c == '_' {
            out.push_str("__");
        } else {
            out.push_str(&format!("_{}_", c as u32));
        }
    }
    out
}

pub fn escape_names(names: &[&str]) -> String {
    let mut out = String::new();
    for (i, name) in names.iter().enumerate() {
        if i > 0 {
            out.push_str("_58_");
        }
        for c in name.chars() {
            if c.is_ascii_alphanumeric() {
                out.push(c);
            } else if c == '_' {
                out.push_str("__");
            } else {
                out.push_str(&format!("_{}_", c as u32));
            }
        }
    }
    out
}

fn escape_string(s: &str) -> String {
    let mut out = "\"".to_string();
    let mut prev_esc = false;
    for c in s.chars() {
        if c.is_ascii_graphic() || c == ' ' {
            if prev_esc && c.is_ascii_hexdigit() {
                out.push_str(&format!("\\x{:01$x}", c as u32, 2));
            } else {
                out.push(c);
                prev_esc = false;
            }
        } else {
            match c.len_utf8() {
                1 => out.push_str(&format!("\\x{:01$x}", c as u32, 2)),
                2 => out.push_str(&format!("\\u{:01$x}", c as u32, 4)),
                3 | 4 => out.push_str(&format!("\\U{:01$x}", c as u32, 8)),
                _ => unreachable!(),
            }
            prev_esc = true;
        }
    }
    out.push('"');
    out
}

use std::collections::HashMap;

use crate::tast;

use super::{
    builtin::{create_init_kind_env, create_init_type_env},
    Typer,
};

#[derive(Debug, Clone, Default)]
pub struct TypeEnv {
    pub env: im::HashMap<String, tast::Type>,
}

impl TypeEnv {
    pub fn new_with_builtin() -> Self {
        let env = create_init_type_env();
        TypeEnv { env }
    }

    pub fn add_type(&mut self, name: String, ty: tast::Type) {
        self.env.insert(name, ty);
    }

    pub fn contain_type(&self, name: &str) -> bool {
        self.env.contains_key(name)
    }

    pub fn get_type(&self, name: &str) -> tast::Type {
        if !self.env.contains_key(name) {
            dbg!(&name);
            panic!()
        }
        self.env.get(name).unwrap().clone()
    }
}

#[derive(Debug, Clone, Default)]
pub struct KindEnv {
    kenv: im::HashMap<String, tast::Type>,
}

impl KindEnv {
    pub fn new_with_builtin() -> Self {
        let kenv = create_init_kind_env();
        KindEnv { kenv }
    }

    pub fn add_kind(&mut self, name: String, ty: tast::Type) {
        self.kenv.insert(name, ty);
    }

    pub fn remove_kind(&mut self, name: &str) {
        self.kenv.remove(name);
    }

    pub fn contain_kind(&self, name: &str) -> bool {
        self.kenv.contains_key(name)
    }

    pub fn get_kind(&self, name: &str) -> tast::Type {
        if let Some(name) = self.kenv.get(name) {
            return name.clone();
        }
        dbg!(&self);
        dbg!(&name);
        panic!("Type not found");
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct P {
    pub index: u32,
}

fn collect_params(ty: &tast::Type, params: &mut Vec<P>) {
    match ty {
        tast::Type::TVar { id: _ } => (),
        tast::Type::Arrow {
            left,
            right,
            generic: _,
        } => {
            for ty in left.iter() {
                collect_params(ty, params);
            }
            collect_params(right, params);
        }
        tast::Type::Param { index, name: _ } => params.push(P { index: *index }),
        tast::Type::Tuple { tys } => {
            for ty in tys.iter() {
                collect_params(ty, params);
            }
        }
        tast::Type::Builtin { ty: _ } => {}
        tast::Type::TConstr {
            tconstr: _,
            tys,
            generic: _,
        } => {
            for ty in tys.iter() {
                collect_params(ty, params);
            }
        }
    }
}

#[derive(Debug, Default)]
pub struct InstEnv {
    pub env: HashMap<u32, tast::Type>,
}

impl InstEnv {
    pub fn from_type(typer: &mut Typer, ty: &tast::Type) -> Self {
        let mut params = vec![];
        collect_params(ty, &mut params);
        params.dedup();
        params.sort();
        let mut env: HashMap<u32, tast::Type> = HashMap::new();
        for (i, _p) in params.iter().enumerate() {
            env.insert(
                i as u32,
                tast::Type::TVar {
                    id: typer.fresh_ty_var(),
                },
            );
        }
        Self { env }
    }
}

impl Typer {
    pub fn enter_scope(&mut self) {
        self.type_env.push(self.type_env.last().unwrap().clone());
        self.kind_env.push(self.kind_env.last().unwrap().clone());
    }

    pub fn exit_scope(&mut self) {
        self.type_env.pop();
        self.kind_env.pop();
    }

    pub fn enter_kind_scope(&mut self) {
        self.kind_env.push(self.kind_env.last().unwrap().clone());
    }

    pub fn exit_kind_scope(&mut self) {
        self.kind_env.pop();
    }

    pub fn get_type_env(&self) -> &TypeEnv {
        self.type_env.last().unwrap()
    }

    pub fn add_type(&mut self, name: String, ty: tast::Type) {
        let last = self.type_env.last_mut().unwrap();
        last.add_type(name, ty)
    }

    pub fn contain_type(&self, name: &str) -> bool {
        let last = self.type_env.last().unwrap();
        last.contain_type(name)
    }

    pub fn get_type(&self, name: &str) -> tast::Type {
        let last = self.type_env.last().unwrap();
        last.get_type(name)
    }

    ////
    pub fn get_kind_env(&self) -> &KindEnv {
        self.kind_env.last().unwrap()
    }

    pub fn add_kind(&mut self, name: String, ty: tast::Type) {
        let last = self.kind_env.last_mut().unwrap();
        last.add_kind(name, ty)
    }

    #[allow(unused)]
    pub fn remove_kind(&mut self, name: &str) {
        let last = self.kind_env.last_mut().unwrap();
        last.remove_kind(name)
    }

    #[allow(unused)]
    pub fn contain_kind(&self, name: &str) -> bool {
        let last = self.kind_env.last().unwrap();
        last.contain_kind(name)
    }

    pub fn get_kind(&self, name: &str) -> tast::Type {
        let last = self.kind_env.last().unwrap();
        last.get_kind(name)
    }
}

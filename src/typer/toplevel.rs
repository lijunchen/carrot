use crate::{ast, tast};

use super::{env::TypeEnv, util::generate_qvars_from_generics, Typer};

impl Typer {
    pub fn toplevel_typer(&mut self, ast: &ast::File) {
        for item in ast.items.iter() {
            match item {
                ast::Item::Fn(ast::Fn {
                    name,
                    generics,
                    params,
                    ret_ty,
                    body: _,
                }) => {
                    self.enter_kind_scope();
                    let qvars = generate_qvars_from_generics(generics);
                    for param in qvars.iter() {
                        if let tast::Type::Param { index: _, name } = param {
                            self.add_kind(name.clone(), param.clone());
                        }
                    }
                    let param_tys: Vec<tast::Type> =
                        params.iter().map(|x| self.tx_type(&x.ty)).collect();
                    let ret_ty = self.tx_type(ret_ty);
                    let func_ty = tast::Type::Arrow {
                        left: param_tys,
                        right: Box::new(ret_ty),
                        generic: !generics.is_empty(),
                    };
                    self.add_type(name.clone(), func_ty);
                    self.exit_kind_scope();
                }
                ast::Item::Enum(ast::Enum {
                    name,
                    generics,
                    variants,
                }) => {
                    let type_env = TypeEnv::default();
                    self.typing_enum(name, generics, variants, type_env);
                }
            }
        }
    }

    fn typing_enum(
        &mut self,
        name: &str,
        generics: &[String],
        variants: &[ast::Variant],
        type_env: TypeEnv,
    ) {
        dbg!(&name);
        dbg!(&generics);
        dbg!(&variants);
        dbg!(&type_env);
        // 1. Create a fresh type constructor μ
        let qvars = generate_qvars_from_generics(generics);
        let tycon = tast::Type::TConstr {
            tconstr: name.into(),
            tys: qvars.clone(),
            generic: !generics.is_empty(),
        };
        self.add_kind(name.into(), tycon.clone());

        // 2. for each value constructor, convert it to type scheme
        {
            self.enter_kind_scope();
            for param in qvars.iter() {
                if let tast::Type::Param { index: _, name } = param {
                    self.add_kind(name.clone(), param.clone());
                }
            }
            dbg!(&self.get_kind_env());

            for v in variants.iter() {
                let mut tys = vec![];
                for ty in v.args.iter() {
                    tys.push(self.tx_type(ty));
                }
                let ret = tycon.clone();
                let vcon_ty = if tys.is_empty() {
                    ret
                } else {
                    tast::Type::Arrow {
                        left: tys,
                        right: Box::new(ret),
                        generic: !generics.is_empty(),
                    }
                };
                self.add_type(v.name.clone(), vcon_ty);
            }

            self.exit_kind_scope();
        }

        dbg!(&self.get_kind_env());
        dbg!(&self.get_type_env());
    }
}

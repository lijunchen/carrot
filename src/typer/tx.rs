use crate::{ast, tast};

use super::Typer;

impl Typer {
    pub fn tx_type(&mut self, ty: &ast::Type) -> tast::Type {
        match ty {
            ast::Type::Name { name } => self.get_kind(name),
            ast::Type::Inst { name, args } => {
                dbg!(&name, args);
                let tycon = &self.get_kind(name);
                if let tast::Type::TConstr {
                    tconstr,
                    tys: _,
                    generic,
                } = tycon
                {
                    let args_ty: Vec<tast::Type> = args.iter().map(|x| self.tx_type(x)).collect();
                    tast::Type::TConstr {
                        tconstr: tconstr.clone(),
                        tys: args_ty,
                        generic: *generic,
                    }
                } else {
                    panic!()
                }
            }
            ast::Type::Missing => tast::Type::TVar {
                id: self.fresh_ty_var(),
            },
            ast::Type::Arrow { left, right } => {
                let left = vec![self.tx_type(left)];
                let right = Box::new(self.tx_type(right));
                tast::Type::Arrow {
                    left,
                    right,
                    generic: false,
                }
            }
            ast::Type::Tuple { items } => {
                let types = items.iter().map(|x| self.tx_type(x)).collect();
                tast::Type::Tuple { tys: types }
            }
        }
    }

    pub fn tx_literal(&self, p: &ast::Literal) -> tast::Literal {
        match p {
            ast::Literal::Bool(x) => tast::Literal::Bool(*x),
            ast::Literal::Int(x) => tast::Literal::Int(*x),
            ast::Literal::String(x) => tast::Literal::String(x.to_string()),
        }
    }

    pub fn tx_param(&mut self, param: &ast::Param) -> tast::Param {
        tast::Param {
            name: param.name.clone(),
            ty: self.tx_type(&param.ty),
        }
    }
}

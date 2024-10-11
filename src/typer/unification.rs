use crate::tast::{self, TypeVar};

use super::{constraint::Constraint, env::InstEnv, Typer};

#[derive(Debug, PartialEq, Eq)]
pub enum TypeError {
    TypeNotEqual(tast::Type, tast::Type),
    InfiniteType(TypeVar, tast::Type),
}

fn occurs_check(var: TypeVar, ty: &tast::Type) -> Result<(), TypeError> {
    match ty {
        tast::Type::TVar { id } => {
            if *id == var {
                Err(TypeError::InfiniteType(var, ty.clone()))
            } else {
                Ok(())
            }
        }
        tast::Type::Arrow {
            left,
            right,
            generic: _,
        } => {
            for ty in left.iter() {
                occurs_check(var, ty)?;
            }
            occurs_check(var, right)
        }
        tast::Type::Param { index: _, name: _ } => Ok(()),
        tast::Type::Builtin { ty: _ } => Ok(()),

        tast::Type::Tuple { tys } => {
            for ty in tys.iter() {
                occurs_check(var, ty)?;
            }
            Ok(())
        }
        tast::Type::TConstr {
            tconstr: _,
            tys,
            generic: _,
        } => {
            for ty in tys.iter() {
                occurs_check(var, ty)?;
            }
            Ok(())
        }
    }
}

impl Typer {
    pub fn fresh_ty_var(&mut self) -> TypeVar {
        self.unification_table.new_key(None)
    }

    pub fn unification(&mut self, constraints: &[Constraint]) -> Result<(), TypeError> {
        for constr in constraints.iter() {
            match constr {
                Constraint::TypeEqual(left, right) => {
                    let result = self.unify_ty_ty(left, right);
                    if let Err(e) = result {
                        self.errors.push(e);
                    }
                }
            }
        }
        Ok(())
    }

    fn normalize_ty(&mut self, ty: &tast::Type) -> tast::Type {
        match ty {
            tast::Type::TVar { id } => match self.unification_table.probe_value(*id) {
                Some(ty) => self.normalize_ty(&ty),
                None => tast::Type::TVar { id: *id },
            },
            tast::Type::Arrow {
                left,
                right,
                generic,
            } => {
                let left = left.iter().map(|x| self.normalize_ty(x)).collect();
                let right = self.normalize_ty(right);
                tast::Type::Arrow {
                    left,
                    right: Box::new(right),
                    generic: *generic,
                }
            }
            tast::Type::Param { index, name } => tast::Type::Param {
                index: *index,
                name: name.clone(),
            },
            tast::Type::Builtin { ty } => tast::Type::Builtin { ty: ty.clone() },
            tast::Type::Tuple { tys } => {
                let tys = tys.iter().map(|x| self.normalize_ty(x)).collect();
                tast::Type::Tuple { tys }
            }
            tast::Type::TConstr {
                tconstr: type_constructor,
                tys,
                generic,
            } => {
                let tys = tys.iter().map(|x| self.normalize_ty(x)).collect();
                tast::Type::TConstr {
                    tconstr: type_constructor.clone(),
                    tys,
                    generic: *generic,
                }
            }
        }
    }

    fn unify_ty_ty(
        &mut self,
        unnorm_left: &tast::Type,
        unnorm_right: &tast::Type,
    ) -> Result<(), TypeError> {
        let left = self.normalize_ty(unnorm_left);
        let right = self.normalize_ty(unnorm_right);

        match (&left, &right) {
            (tast::Type::Builtin { ty: ty_left }, tast::Type::Builtin { ty: ty_right }) => {
                if ty_left == ty_right {
                    Ok(())
                } else {
                    Err(TypeError::TypeNotEqual(
                        tast::Type::Builtin {
                            ty: ty_left.clone(),
                        },
                        tast::Type::Builtin {
                            ty: ty_right.clone(),
                        },
                    ))
                }
            }
            (
                tast::Type::Arrow {
                    left: argsl,
                    right: retl,
                    generic: generic1,
                },
                tast::Type::Arrow {
                    left: argsr,
                    right: retr,
                    generic: generic2,
                },
            ) => {
                if generic1 != generic2 {
                    dbg!(&left, &right);
                    panic!()
                }
                if argsl.len() != argsr.len() {
                    unimplemented!()
                }
                for (l, r) in argsl.iter().zip(argsr.iter()) {
                    self.unify_ty_ty(l, r)?;
                }
                self.unify_ty_ty(retl, retr)
            }
            (tast::Type::TVar { id: a }, tast::Type::TVar { id: b }) => self
                .unification_table
                .unify_var_var(*a, *b)
                .map_err(|(l, r)| TypeError::TypeNotEqual(l, r)),
            (tast::Type::TVar { id }, ty) | (ty, tast::Type::TVar { id }) => {
                occurs_check(*id, ty)?;
                self.unification_table
                    .unify_var_value(*id, Some(ty.clone()))
                    .map_err(|(l, r)| TypeError::TypeNotEqual(l, r))
            }
            (tast::Type::Tuple { tys: tys_l }, tast::Type::Tuple { tys: tys_r }) => {
                if tys_l.len() != tys_r.len() {
                    unimplemented!()
                }
                for (l, r) in tys_l.iter().zip(tys_r.iter()) {
                    self.unify_ty_ty(l, r)?;
                }
                Ok(())
            }
            (
                tast::Type::TConstr {
                    tconstr: left_type_constructor,
                    tys: left_tys,
                    generic: left_generic,
                },
                tast::Type::TConstr {
                    tconstr: right_type_constructor,
                    tys: right_tys,
                    generic: right_generic,
                },
            ) => {
                if left_type_constructor != right_type_constructor {
                    return Err(TypeError::TypeNotEqual(left.clone(), right.clone()));
                }
                if left_tys.len() != right_tys.len() {
                    return Err(TypeError::TypeNotEqual(left.clone(), right.clone()));
                }
                if left_generic != right_generic {
                    return Err(TypeError::TypeNotEqual(left.clone(), right.clone()));
                }
                for (l, r) in left_tys.iter().zip(right_tys.iter()) {
                    self.unify_ty_ty(l, r)?;
                }
                Ok(())
            }
            (
                tast::Type::Param {
                    index: left_index,
                    name: left_name,
                },
                tast::Type::Param {
                    index: right_index,
                    name: right_name,
                },
            ) => {
                if left_index != right_index {
                    return Err(TypeError::TypeNotEqual(left.clone(), right.clone()));
                }
                if left_name != right_name {
                    return Err(TypeError::TypeNotEqual(left.clone(), right.clone()));
                }
                Ok(())
            }
            _ => Err(TypeError::TypeNotEqual(left, right)),
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    fn _inst(&mut self, ty: &tast::Type, inst_env: &InstEnv) -> tast::Type {
        fn go(typer: &mut Typer, ty: &tast::Type, inst_env: &InstEnv) -> tast::Type {
            match ty {
                tast::Type::TVar { id } => tast::Type::TVar { id: *id },
                tast::Type::Arrow {
                    left,
                    right,
                    generic: _,
                } => {
                    let left = left.iter().map(|x| go(typer, x, inst_env)).collect();
                    let right = Box::new(go(typer, right, inst_env));
                    tast::Type::Arrow {
                        left,
                        right,
                        generic: false,
                    }
                }
                tast::Type::Param { index, name: _ } => {
                    if !inst_env.env.contains_key(index) {
                        panic!("Type parameter not found")
                    }
                    let ty = inst_env.env.get(index).unwrap().clone();
                    ty
                }
                tast::Type::Tuple { tys } => {
                    let tys = tys.iter().map(|x| go(typer, x, inst_env)).collect();
                    tast::Type::Tuple { tys }
                }
                tast::Type::Builtin { ty } => {
                    let ty = ty.clone();
                    tast::Type::Builtin { ty }
                }
                tast::Type::TConstr {
                    tconstr: type_constructor,
                    tys,
                    generic,
                } => {
                    let tys = tys.iter().map(|x| go(typer, x, inst_env)).collect();

                    tast::Type::TConstr {
                        tconstr: type_constructor.into(),
                        tys,
                        generic: *generic,
                    }
                }
            }
        }

        go(self, ty, inst_env)
    }

    pub fn inst(&mut self, ty: &tast::Type) -> tast::Type {
        let inst_env = InstEnv::from_type(self, ty);

        self._inst(ty, &inst_env)
    }
}

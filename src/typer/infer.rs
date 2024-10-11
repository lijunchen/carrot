use crate::{ast, tast};

use crate::typer::Typer;

use super::builtin::{T_BOOL, T_INT, T_STRING, T_UNIT};
use super::constraint::Constraint;
use super::util::generate_qvars_from_generics;

#[derive(Debug)]
pub enum TypedStmtOrExpr {
    TypedStmt(tast::Stmt),
    TypedExpr(tast::Expr),
    TypedPattern(tast::Pattern),
}

impl TypedStmtOrExpr {
    pub fn to_typed_expr(&self) -> tast::Expr {
        match self {
            TypedStmtOrExpr::TypedExpr(expr) => expr.clone(),
            _ => unreachable!(),
        }
    }

    #[allow(unused)]
    fn to_typed_stmt(&self) -> tast::Stmt {
        match self {
            TypedStmtOrExpr::TypedStmt(stmt) => stmt.clone(),
            _ => unreachable!(),
        }
    }

    fn to_typed_pattern(&self) -> tast::Pattern {
        match self {
            TypedStmtOrExpr::TypedPattern(p) => p.clone(),
            _ => unreachable!(),
        }
    }

    fn get_type(&self) -> tast::Type {
        match self {
            TypedStmtOrExpr::TypedStmt(stmt) => match stmt {
                tast::Stmt::LetStmt {
                    pat: _,
                    ty,
                    value: _,
                } => ty.clone(),
                tast::Stmt::ExprStmt { expr } => expr.get_type(),
            },
            TypedStmtOrExpr::TypedExpr(expr) => expr.get_type(),
            TypedStmtOrExpr::TypedPattern(pat) => pat.get_type(),
        }
    }
}

#[derive(Debug)]
pub struct InferOut {
    pub constraints: Vec<Constraint>,
    pub typed_ast: TypedStmtOrExpr,
}

impl InferOut {
    pub fn new(constraints: Vec<Constraint>, typed_ast: TypedStmtOrExpr) -> Self {
        InferOut {
            constraints,
            typed_ast,
        }
    }

    pub fn new_typed_stmt(constraints: Vec<Constraint>, typed_stmt: tast::Stmt) -> Self {
        InferOut {
            constraints,
            typed_ast: TypedStmtOrExpr::TypedStmt(typed_stmt),
        }
    }

    pub fn new_typed_expr(constraints: Vec<Constraint>, typed_expr: tast::Expr) -> Self {
        InferOut {
            constraints,
            typed_ast: TypedStmtOrExpr::TypedExpr(typed_expr),
        }
    }

    pub fn to_typed_expr(self) -> tast::Expr {
        self.typed_ast.to_typed_expr()
    }

    #[allow(unused)]
    pub fn to_typed_stmt(self) -> tast::Stmt {
        match self.typed_ast {
            TypedStmtOrExpr::TypedStmt(stmt) => stmt,
            _ => unreachable!(),
        }
    }
}

impl Typer {
    pub fn infer(&mut self, ast: &ast::File) -> tast::File {
        self.toplevel_typer(ast);
        let mut items = vec![];
        let mut constraints = vec![];
        for item in ast.items.iter() {
            match item {
                ast::Item::Fn(f) => {
                    self.enter_scope();
                    let (cs, typed_item) = self.infer_fn(f);
                    constraints.extend(cs);
                    items.push(tast::Item::Fn(typed_item));
                    self.exit_scope();
                }
                ast::Item::Enum { .. } => (),
            };
        }
        let tast = tast::File { items };
        self.unification(&constraints).unwrap();
        dbg!(&constraints);
        let tast = self.subst_ast(tast);
        dbg!(&self.errors);
        tast
    }

    fn infer_fn(&mut self, node: &ast::Fn) -> (Vec<Constraint>, tast::Fn) {
        let ast::Fn {
            name,
            generics,
            params,
            ret_ty,
            body,
        } = node;
        self.enter_scope();
        let qvars = generate_qvars_from_generics(generics);
        for param in qvars.iter() {
            if let tast::Type::Param { index: _, name } = param {
                self.add_kind(name.clone(), param.clone());
            }
        }
        let mut constraints = vec![];
        let return_ty = self.tx_type(ret_ty);
        if name == "main" {
            constraints.push(Constraint::TypeEqual(return_ty.clone(), T_UNIT));
        }
        for param in params.iter() {
            let t = self.tx_type(&param.ty);
            self.add_type(param.name.clone(), t);
        }

        let (typed_body, body_ty) = self.infer_expr(body);
        for c in typed_body.constraints.iter() {
            constraints.push(c.clone());
        }
        let typed_fn = tast::Fn {
            name: name.clone(),
            generics: generics.clone(),
            params: params.iter().map(|x| self.tx_param(x)).collect(),
            ret_ty: return_ty.clone(),
            body: Box::new(typed_body.to_typed_expr()),
        };
        constraints.push(Constraint::TypeEqual(body_ty, return_ty));
        self.exit_scope();
        (constraints, typed_fn)
    }

    fn check_pattern(&mut self, node: &ast::Pattern, typ: &tast::Type) -> InferOut {
        let mut constraints = vec![];
        let typed_ast = match node {
            ast::Pattern::Literal { pat } => {
                let spat = self.tx_literal(pat);
                let one_ty = tast::Type::TVar {
                    id: self.fresh_ty_var(),
                };
                constraints.push(Constraint::TypeEqual(spat.get_type(), one_ty.clone()));
                let typed_pat = tast::Pattern::Literal {
                    ty: spat.get_type(),
                    pat: spat,
                };
                constraints.push(Constraint::TypeEqual(typed_pat.get_type(), one_ty.clone()));
                constraints.push(Constraint::TypeEqual(typ.clone(), one_ty.clone()));
                TypedStmtOrExpr::TypedPattern(typed_pat)
            }
            ast::Pattern::Tuple { pats } => {
                let one_ty = tast::Type::TVar {
                    id: self.fresh_ty_var(),
                };
                let mut typed_pats = vec![];
                let mut type_of_typed_pats = vec![];
                for pat in pats.iter() {
                    let guess_ty = tast::Type::TVar {
                        id: self.fresh_ty_var(),
                    };
                    let InferOut {
                        constraints: cs,
                        typed_ast,
                    } = self.check_pattern(pat, &guess_ty);
                    constraints.extend(cs);
                    type_of_typed_pats.push(typed_ast.get_type());
                    typed_pats.push(typed_ast.to_typed_pattern());
                }
                let final_type = tast::Type::Tuple {
                    tys: type_of_typed_pats,
                };
                constraints.push(Constraint::TypeEqual(typ.clone(), final_type.clone()));
                constraints.push(Constraint::TypeEqual(typ.clone(), one_ty.clone()));
                TypedStmtOrExpr::TypedPattern(tast::Pattern::Tuple {
                    pats: typed_pats,
                    ty: final_type,
                })
            }
            ast::Pattern::Ident { name } => {
                let typ1 = tast::Type::TVar {
                    id: self.fresh_ty_var(),
                };
                let typ1_inst = self.inst(&typ1);
                self.add_type(name.clone(), typ1_inst.clone());
                constraints.push(Constraint::TypeEqual(typ.clone(), typ1_inst.clone()));
                TypedStmtOrExpr::TypedPattern(tast::Pattern::Ident {
                    name: name.clone(),
                    ty: typ1_inst,
                })
            }
            ast::Pattern::Wild => {
                let one_ty = tast::Type::TVar {
                    id: self.fresh_ty_var(),
                };
                constraints.push(Constraint::TypeEqual(typ.clone(), one_ty.clone()));
                TypedStmtOrExpr::TypedPattern(tast::Pattern::Wild { ty: one_ty.clone() })
            }
            ast::Pattern::Constructor { name, args } => {
                let vconstr_ty = self.inst(&self.get_type(name));
                let mut typed_args = vec![];
                let mut type_of_typed_args = vec![];
                match &vconstr_ty {
                    tast::Type::Arrow {
                        left,
                        right,
                        generic: _,
                    } => {
                        if left.len() != args.len() {
                            panic!()
                        }
                        for (i, arg) in args.iter().enumerate() {
                            let InferOut {
                                constraints: cs,
                                typed_ast,
                            } = self.check_pattern(arg, &left[i]);
                            type_of_typed_args.push(typed_ast.get_type());
                            typed_args.push(typed_ast.to_typed_pattern());
                            constraints.extend(cs);
                            constraints
                                .push(Constraint::TypeEqual(typed_ast.get_type(), left[i].clone()));
                        }
                        constraints.push(Constraint::TypeEqual(typ.clone(), *right.clone()));
                    }
                    tast::Type::TConstr {
                        tconstr: _,
                        tys: _,
                        generic: _,
                    } => {
                        if !args.is_empty() {
                            panic!()
                        }
                        constraints.push(Constraint::TypeEqual(typ.clone(), vconstr_ty.clone()));
                    }
                    _ => unreachable!(),
                }

                let final_type = match &vconstr_ty {
                    tast::Type::Arrow {
                        left: _,
                        right,
                        generic: _,
                    } => *right.clone(),
                    tast::Type::TConstr {
                        tconstr: _,
                        tys: _,
                        generic: _,
                    } => vconstr_ty.clone(),
                    _ => unreachable!(),
                };
                TypedStmtOrExpr::TypedPattern(tast::Pattern::Constructor {
                    name: name.clone(),
                    args: typed_args,
                    ty: final_type,
                })
            }
        };

        InferOut {
            constraints,
            typed_ast,
        }
    }

    fn infer_stmt(&mut self, node: &ast::Stmt) -> (InferOut, tast::Type) {
        match node {
            ast::Stmt::LetStmt {
                pat,
                ty: annotation_ty,
                value,
            } => match annotation_ty {
                ast::Type::Missing => {
                    let (infer_out1, value_ty) = Typer::infer_expr(self, value);
                    let mut constraints: Vec<Constraint> = vec![];
                    let ty = match annotation_ty {
                        ast::Type::Missing => value_ty.clone(),
                        _ => self.tx_type(annotation_ty),
                    };
                    constraints.extend(infer_out1.constraints);
                    constraints.push(Constraint::TypeEqual(value_ty.clone(), ty.clone()));
                    let infer_out2 = self.check_pattern(pat, &value_ty);
                    constraints.extend(infer_out2.constraints);
                    let typed_pattern = match infer_out2.typed_ast {
                        TypedStmtOrExpr::TypedPattern(typed_pattern) => typed_pattern,
                        _ => unreachable!(),
                    };
                    constraints.push(Constraint::TypeEqual(typed_pattern.get_type(), ty.clone()));
                    let infer_out3 = InferOut::new_typed_stmt(
                        constraints,
                        tast::Stmt::LetStmt {
                            pat: typed_pattern,
                            ty: ty.clone(),
                            value: infer_out1.typed_ast.to_typed_expr(),
                        },
                    );
                    (infer_out3, ty)
                }
                _ => {
                    let annot_ty = self.tx_type(annotation_ty);
                    dbg!(&annot_ty);

                    let InferOut {
                        constraints,
                        typed_ast: typed_value,
                    } = self.check_expr(value, &annot_ty);
                    let mut constraints = constraints;
                    constraints.push(Constraint::TypeEqual(
                        typed_value.to_typed_expr().get_type(),
                        annot_ty.clone(),
                    ));
                    let InferOut {
                        constraints: pat_constraints,
                        typed_ast: spat,
                    } = self.check_pattern(pat, &annot_ty);
                    let typed_ast = TypedStmtOrExpr::TypedStmt(tast::Stmt::LetStmt {
                        pat: spat.to_typed_pattern(),
                        ty: annot_ty.clone(),
                        value: typed_value.to_typed_expr(),
                    });
                    constraints.extend(pat_constraints);
                    constraints.push(Constraint::TypeEqual(spat.get_type(), annot_ty.clone()));
                    (
                        InferOut {
                            constraints,
                            typed_ast,
                        },
                        annot_ty,
                    )
                }
            },
            ast::Stmt::ExprStmt { expr } => {
                let (infer_out, ty) = Typer::infer_expr(self, expr);
                let infer_out = InferOut {
                    constraints: infer_out.constraints.clone(),
                    typed_ast: TypedStmtOrExpr::TypedStmt(tast::Stmt::ExprStmt {
                        expr: infer_out.to_typed_expr(),
                    }),
                };
                (infer_out, ty)
            }
        }
    }

    fn check_stmt(&mut self, node: &ast::Stmt, _ty: &tast::Type) -> InferOut {
        match node {
            ast::Stmt::LetStmt {
                pat: _,
                ty: _,
                value: _,
            } => todo!(),
            ast::Stmt::ExprStmt { expr: _ } => {
                todo!()
            }
        }
    }

    fn infer_literal(&mut self, node: &ast::Literal) -> (InferOut, tast::Type) {
        match node {
            ast::Literal::Bool(x) => (
                InferOut::new_typed_expr(
                    vec![],
                    tast::Expr::Literal {
                        value: tast::Literal::Bool(*x),
                        ty: T_BOOL,
                    },
                ),
                T_BOOL,
            ),
            ast::Literal::Int(x) => (
                InferOut::new_typed_expr(
                    vec![],
                    tast::Expr::Literal {
                        value: tast::Literal::Int(*x),
                        ty: T_INT,
                    },
                ),
                T_INT,
            ),
            ast::Literal::String(x) => (
                InferOut::new_typed_expr(
                    vec![],
                    tast::Expr::Literal {
                        value: tast::Literal::String(x.to_string()),
                        ty: T_STRING,
                    },
                ),
                T_STRING,
            ),
        }
    }

    fn infer_expr(&mut self, node: &ast::Expr) -> (InferOut, tast::Type) {
        match node {
            ast::Expr::Literal { value } => self.infer_literal(value),
            ast::Expr::Name { name } => {
                let ty = if !self.contain_type(name) {
                    tast::Type::TVar {
                        id: self.fresh_ty_var(),
                    }
                } else {
                    self.get_type(name)
                };
                let ty_inst = self.inst(&ty);
                (
                    InferOut::new_typed_expr(
                        vec![],
                        tast::Expr::Name {
                            name: name.clone(),
                            ty: ty_inst.clone(),
                        },
                    ),
                    ty_inst,
                )
            }
            ast::Expr::VCon { name } => {
                let vconstr = self.get_type(name);
                let constraints = vec![];
                let ty_inst = self.inst(&vconstr);
                (
                    InferOut::new_typed_expr(
                        constraints,
                        tast::Expr::VCon {
                            name: name.clone(),
                            ty: ty_inst.clone(),
                        },
                    ),
                    ty_inst,
                )
            }
            ast::Expr::Block { stmts, expr } => {
                let mut typed_stmts = vec![];
                let mut constraints = vec![];
                for stmt in stmts.iter() {
                    let (infer_out, _out_ty) = Typer::infer_stmt(self, stmt);
                    constraints.extend(infer_out.constraints);
                    if let TypedStmtOrExpr::TypedStmt(typed_stmt) = infer_out.typed_ast {
                        typed_stmts.push(typed_stmt);
                    } else {
                        unreachable!()
                    }
                }

                if let Some(tail_expr) = expr {
                    let (infer_out, ty) = self.infer_expr(tail_expr);
                    let typed_expr = infer_out.typed_ast.to_typed_expr();
                    constraints.extend(infer_out.constraints);

                    (
                        InferOut::new_typed_expr(
                            constraints,
                            tast::Expr::Block {
                                stmts: typed_stmts,
                                expr: Some(Box::new(typed_expr)),
                                ty: ty.clone(),
                            },
                        ),
                        ty,
                    )
                } else {
                    (
                        InferOut::new_typed_expr(
                            constraints,
                            tast::Expr::Block {
                                stmts: typed_stmts,
                                expr: None,
                                ty: T_UNIT.clone(),
                            },
                        ),
                        T_UNIT.clone(),
                    )
                }
            }
            ast::Expr::Call { f, args } => {
                let (
                    InferOut {
                        constraints: infer_out_callee_constr,
                        typed_ast: infer_out_callee_typed_ast,
                    },
                    callee_ty,
                ) = self.infer_expr(f);

                let mut tys = vec![];
                let mut typed_args = vec![];
                let mut constraints = vec![];
                constraints.extend(infer_out_callee_constr);
                for arg in args.iter() {
                    let (infer_out_arg, arg_ty) = self.infer_expr(arg);
                    constraints.extend(infer_out_arg.constraints);
                    tys.push(arg_ty);
                    if let TypedStmtOrExpr::TypedExpr(typed_expr) = infer_out_arg.typed_ast {
                        typed_args.push(typed_expr);
                    } else {
                        unreachable!()
                    }
                }
                let ty = tast::Type::TVar {
                    id: self.fresh_ty_var(),
                };
                constraints.push(Constraint::TypeEqual(
                    callee_ty,
                    tast::Type::Arrow {
                        left: tys,
                        right: Box::new(ty.clone()),
                        generic: false,
                    },
                ));
                (
                    InferOut::new_typed_expr(
                        constraints,
                        tast::Expr::Call {
                            callee: Box::new(infer_out_callee_typed_ast.to_typed_expr()),
                            args: typed_args,
                            ty: ty.clone(),
                        },
                    ),
                    ty,
                )
            }
            ast::Expr::If { cond, then, orelse } => {
                let (
                    InferOut {
                        constraints: infer_out_cond_constr,
                        typed_ast: infer_out_cond_typed_ast,
                    },
                    ty_cond,
                ) = Typer::infer_expr(self, cond);
                let (
                    InferOut {
                        constraints: infer_out_then_constr,
                        typed_ast: infer_out_then_typed_ast,
                    },
                    ty_then,
                ) = Typer::infer_expr(self, then);
                let (
                    InferOut {
                        constraints: infer_out_orelse_constr,
                        typed_ast: infer_out_orelse_typed_ast,
                    },
                    ty_orelse,
                ) = Typer::infer_expr(self, orelse);

                let ty = tast::Type::TVar {
                    id: self.fresh_ty_var(),
                };
                let e = tast::Expr::If {
                    cond: Box::new(infer_out_cond_typed_ast.to_typed_expr()),
                    then: Box::new(infer_out_then_typed_ast.to_typed_expr()),
                    orelse: Box::new(infer_out_orelse_typed_ast.to_typed_expr()),
                    ty: ty.clone(),
                };

                let mut constraints: Vec<Constraint> = vec![];
                constraints.extend(infer_out_cond_constr);
                constraints.extend(infer_out_then_constr);
                constraints.extend(infer_out_orelse_constr);
                constraints.push(Constraint::TypeEqual(ty_cond, T_BOOL));
                constraints.push(Constraint::TypeEqual(ty_then, ty.clone()));
                constraints.push(Constraint::TypeEqual(ty_orelse, ty.clone()));

                (InferOut::new_typed_expr(constraints, e), ty)
            }
            ast::Expr::Unit => {
                let ty = T_UNIT;
                (
                    InferOut::new_typed_expr(vec![], tast::Expr::Unit { ty: ty.clone() }),
                    ty,
                )
            }
            ast::Expr::Tuple { items } => {
                let mut typed_exprs = vec![];
                let mut constraints = vec![];
                let mut tys = vec![];
                for expr in items.iter() {
                    let (infer_out, expr_ty) = Typer::infer_expr(self, expr);
                    constraints.extend(infer_out.constraints);
                    if let TypedStmtOrExpr::TypedExpr(typed_expr) = infer_out.typed_ast {
                        typed_exprs.push(typed_expr);
                        tys.push(expr_ty);
                    } else {
                        unreachable!()
                    }
                }
                let ty = tast::Type::Tuple { tys };
                (
                    InferOut::new_typed_expr(
                        constraints,
                        tast::Expr::Tuple {
                            exprs: typed_exprs,
                            ty: ty.clone(),
                        },
                    ),
                    ty,
                )
            }
            ast::Expr::Match { expr, arms } => {
                let (
                    InferOut {
                        constraints: expr_constraints,
                        typed_ast: typed_expr,
                    },
                    expr_ty,
                ) = Typer::infer_expr(self, expr);

                let mut typed_arms = vec![];
                let mut constraints = vec![];
                constraints.extend(expr_constraints);
                let match_result_ty = tast::Type::TVar {
                    id: self.fresh_ty_var(),
                };
                for (pattern, arm) in arms.iter() {
                    let InferOut {
                        constraints: pattern_contraints,
                        typed_ast: typed_pattern,
                    } = self.check_pattern(pattern, &expr_ty);
                    constraints.extend(pattern_contraints.into_iter());
                    let (
                        InferOut {
                            constraints: arm_constraints,
                            typed_ast: typed_arm,
                        },
                        arm_typ,
                    ) = Typer::infer_expr(self, arm);
                    constraints.extend(arm_constraints.into_iter());
                    typed_arms.push((typed_pattern.to_typed_pattern(), typed_arm.to_typed_expr()));
                    constraints.push(Constraint::TypeEqual(match_result_ty.clone(), arm_typ));
                }
                (
                    InferOut::new_typed_expr(
                        constraints,
                        tast::Expr::Match {
                            expr: Box::new(typed_expr.to_typed_expr()),
                            arms: typed_arms,
                            ty: match_result_ty.clone(),
                        },
                    ),
                    match_result_ty,
                )
            }
        }
    }

    fn check_literal(&mut self, node: &ast::Literal, ty: &tast::Type) -> InferOut {
        match (node, ty) {
            (ast::Literal::Bool(x), &T_BOOL) => InferOut::new_typed_expr(
                vec![],
                tast::Expr::Literal {
                    value: tast::Literal::Bool(*x),
                    ty: T_BOOL,
                },
            ),

            (ast::Literal::Int(x), &T_INT) => InferOut::new_typed_expr(
                vec![],
                tast::Expr::Literal {
                    value: tast::Literal::Int(*x),
                    ty: T_INT,
                },
            ),
            _ => {
                let s_lit = self.tx_literal(node);
                let constraints = vec![Constraint::TypeEqual(s_lit.get_type(), ty.clone())];
                InferOut::new(
                    constraints,
                    TypedStmtOrExpr::TypedExpr(tast::Expr::Literal {
                        value: s_lit,
                        ty: ty.clone(),
                    }),
                )
            }
        }
    }

    fn check_expr(&mut self, node: &ast::Expr, ty: &tast::Type) -> InferOut {
        match (node, ty) {
            (ast::Expr::Literal { value }, _) => self.check_literal(value, ty),
            (ast::Expr::Name { name }, _) => {
                let name_ty = if self.contain_type(name) {
                    self.get_type(name)
                } else {
                    tast::Type::TVar {
                        id: self.fresh_ty_var(),
                    }
                };
                let ty_inst = self.inst(&name_ty);
                let constraints = vec![Constraint::TypeEqual(ty_inst.clone(), ty.clone())];
                InferOut::new(
                    constraints,
                    TypedStmtOrExpr::TypedExpr(tast::Expr::Name {
                        name: name.clone(),
                        ty: ty_inst.clone(),
                    }),
                )
            }
            (ast::Expr::VCon { name }, ty) => {
                let vconstr = self.get_type(name);
                let mut constraints = vec![];
                let ty_inst = self.inst(&vconstr);
                constraints.push(Constraint::TypeEqual(ty_inst.clone(), ty.clone()));
                InferOut::new_typed_expr(
                    constraints,
                    tast::Expr::VCon {
                        name: name.clone(),
                        ty: ty_inst.clone(),
                    },
                )
            }
            (ast::Expr::Block { stmts, expr }, ty) => {
                let mut typed_stmts = vec![];
                let mut constraints = vec![];
                for stmt in stmts.iter() {
                    let infer_out = Typer::check_stmt(self, stmt, ty);
                    constraints.extend(infer_out.constraints);
                    if let TypedStmtOrExpr::TypedStmt(typed_stmt) = infer_out.typed_ast {
                        typed_stmts.push(typed_stmt);
                    } else {
                        unreachable!()
                    }
                }
                if let Some(tail_expr) = expr {
                    let infer_out = Typer::check_expr(self, tail_expr, ty);
                    constraints.extend(infer_out.constraints);
                    let type_expr = infer_out.typed_ast.to_typed_expr();
                    InferOut::new_typed_expr(
                        constraints,
                        tast::Expr::Block {
                            stmts: typed_stmts,
                            expr: Some(Box::new(type_expr)),
                            ty: ty.clone(),
                        },
                    )
                } else {
                    InferOut::new_typed_expr(
                        constraints,
                        tast::Expr::Block {
                            stmts: typed_stmts,
                            expr: None,
                            ty: T_UNIT,
                        },
                    )
                }
            }
            (ast::Expr::Call { f, args }, _) => {
                // First, infer the type and constraints of the callee expression
                let callee_infer_out = Typer::infer_expr(self, f);
                let expected_arg_types = if let tast::Type::Arrow {
                    left,
                    right: _,
                    generic: _,
                } = &callee_infer_out.1
                {
                    left.clone()
                } else {
                    panic!(
                        "Expected a function type for the callee, found {:?}",
                        callee_infer_out.1
                    );
                };

                // Now, check each argument against the expected type from the function signature
                let mut arg_constraints = vec![];
                let mut typed_args = vec![];
                for (arg, expected_type) in args.iter().zip(expected_arg_types.iter()) {
                    let arg_infer_out = Typer::check_expr(self, arg, expected_type);
                    arg_constraints.extend(arg_infer_out.constraints);
                    if let TypedStmtOrExpr::TypedExpr(typed_expr) = arg_infer_out.typed_ast {
                        typed_args.push(typed_expr);
                    } else {
                        unreachable!();
                    }
                }

                // Assuming the return type of the callee is the right part of the Arrow type
                let return_type = if let tast::Type::Arrow {
                    left: _,
                    right,
                    generic: _,
                } = &callee_infer_out.1
                {
                    *right.clone()
                } else {
                    panic!(
                        "Expected a function type for the callee, found {:?}",
                        callee_infer_out.1
                    );
                };

                // Combine all constraints: from the callee and all arguments
                let mut all_constraints = vec![];
                all_constraints.extend(callee_infer_out.0.constraints);
                all_constraints.extend(arg_constraints);

                // Create the typed CallExpr with all typed arguments and the inferred return type
                InferOut::new_typed_expr(
                    all_constraints,
                    tast::Expr::Call {
                        callee: Box::new(callee_infer_out.0.typed_ast.to_typed_expr()),
                        args: typed_args,
                        ty: return_type,
                    },
                )
            }
            (ast::Expr::If { cond, then, orelse }, _) => {
                let InferOut {
                    constraints: cond_constr,
                    typed_ast: cond_tast,
                } = Typer::check_expr(self, cond, &T_BOOL);
                let InferOut {
                    constraints: then_constr,
                    typed_ast: then_tast,
                } = Typer::check_expr(self, then, ty);
                let InferOut {
                    constraints: orelse_constr,
                    typed_ast: orelse_tast,
                } = Typer::check_expr(self, orelse, ty);
                let mut constraints = vec![];
                constraints.extend(cond_constr);
                constraints.extend(then_constr);
                constraints.extend(orelse_constr);
                InferOut::new_typed_expr(
                    constraints,
                    tast::Expr::If {
                        cond: Box::new(cond_tast.to_typed_expr()),
                        then: Box::new(then_tast.to_typed_expr()),
                        orelse: Box::new(orelse_tast.to_typed_expr()),
                        ty: ty.clone(),
                    },
                )
            }
            (ast::Expr::Unit, _) => {
                let constraints = vec![Constraint::TypeEqual(T_UNIT, ty.clone())];
                InferOut::new_typed_expr(constraints, tast::Expr::Unit { ty: T_UNIT })
            }
            (ast::Expr::Tuple { items }, ty) => match ty {
                tast::Type::Tuple { tys } => {
                    let mut typed_exprs = vec![];
                    let mut constraints = vec![];
                    for (expr, ty) in items.iter().zip(tys.iter()) {
                        let infer_out = Typer::check_expr(self, expr, ty);
                        constraints.extend(infer_out.constraints);
                        if let TypedStmtOrExpr::TypedExpr(typed_expr) = infer_out.typed_ast {
                            typed_exprs.push(typed_expr);
                        } else {
                            unreachable!()
                        }
                    }
                    InferOut::new_typed_expr(
                        constraints,
                        tast::Expr::Tuple {
                            exprs: typed_exprs,
                            ty: ty.clone(),
                        },
                    )
                }
                _ => todo!(),
            },
            (ast::Expr::Match { expr, arms }, ty) => {
                let (
                    InferOut {
                        constraints: expr_constraints,
                        typed_ast: typed_expr,
                    },
                    expr_ty,
                ) = Typer::infer_expr(self, expr);

                let mut typed_arms = vec![];
                let mut constraints = vec![];
                constraints.extend(expr_constraints);
                for (pattern, arm) in arms.iter() {
                    let InferOut {
                        constraints: pattern_constraints,
                        typed_ast: typed_pattern,
                    } = self.check_pattern(pattern, &expr_ty);
                    constraints.extend(pattern_constraints.into_iter());
                    let InferOut {
                        constraints: arm_constraints,
                        typed_ast: typed_arm,
                    } = Typer::check_expr(self, arm, ty);
                    constraints.extend(arm_constraints.into_iter());
                    typed_arms.push((typed_pattern.to_typed_pattern(), typed_arm.to_typed_expr()));
                    constraints.push(Constraint::TypeEqual(typed_arm.get_type(), ty.clone()));
                }

                InferOut::new_typed_expr(
                    constraints,
                    tast::Expr::Match {
                        expr: Box::new(typed_expr.to_typed_expr()),
                        arms: typed_arms,
                        ty: ty.clone(),
                    },
                )
            }
        }
    }
}

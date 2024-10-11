use crate::tast;

use super::Typer;

impl Typer {
    fn subst_type(&mut self, ty: &tast::Type) -> tast::Type {
        match ty {
            tast::Type::TVar { id } => match self.unification_table.probe_value(*id) {
                Some(t) => t,
                None => tast::Type::TVar { id: *id },
            },
            tast::Type::Arrow {
                left,
                right,
                generic,
            } => tast::Type::Arrow {
                left: left.iter().map(|t| self.subst_type(t)).collect(),
                right: Box::new(self.subst_type(right)),
                generic: *generic,
            },
            tast::Type::Param { index, name } => tast::Type::Param {
                index: *index,
                name: name.clone(),
            },
            tast::Type::Builtin { ty } => tast::Type::Builtin { ty: ty.clone() },
            tast::Type::Tuple { tys } => tast::Type::Tuple {
                tys: tys.iter().map(|t| self.subst_type(t)).collect(),
            },
            tast::Type::TConstr {
                tconstr: type_constructor,
                tys,
                generic,
            } => tast::Type::TConstr {
                tconstr: type_constructor.clone(),
                tys: tys.iter().map(|t| self.subst_type(t)).collect(),
                generic: *generic,
            },
        }
    }

    fn subst_pattern(&mut self, pattern: &tast::Pattern) -> tast::Pattern {
        match pattern {
            tast::Pattern::Literal { pat, ty } => tast::Pattern::Literal {
                pat: pat.clone(),
                ty: self.subst_type(ty),
            },
            tast::Pattern::Tuple { pats, ty } => tast::Pattern::Tuple {
                pats: pats.iter().map(|p| self.subst_pattern(p)).collect(),
                ty: self.subst_type(ty),
            },
            tast::Pattern::Ident { name, ty } => tast::Pattern::Ident {
                name: name.clone(),
                ty: self.subst_type(ty),
            },
            tast::Pattern::Wild { ty } => tast::Pattern::Wild {
                ty: self.subst_type(ty),
            },
            tast::Pattern::Constructor { name, args, ty } => tast::Pattern::Constructor {
                name: name.clone(),
                args: args.iter().map(|p| self.subst_pattern(p)).collect(),
                ty: self.subst_type(ty),
            },
        }
    }

    fn subst_expr(&mut self, expr: &tast::Expr) -> tast::Expr {
        match expr {
            tast::Expr::Literal { value, ty } => tast::Expr::Literal {
                value: value.clone(),
                ty: self.subst_type(ty),
            },
            tast::Expr::Name { name, ty } => tast::Expr::Name {
                name: name.clone(),
                ty: self.subst_type(ty),
            },
            tast::Expr::VCon { name, ty } => tast::Expr::VCon {
                name: name.clone(),
                ty: self.subst_type(ty),
            },
            tast::Expr::Block { stmts, expr, ty } => tast::Expr::Block {
                stmts: stmts.iter().map(|s| self.subst_stmt(s)).collect(),
                expr: expr.as_deref().map(|e| Box::new(self.subst_expr(e))),
                ty: self.subst_type(ty),
            },
            tast::Expr::Call { callee, args, ty } => tast::Expr::Call {
                callee: Box::new(self.subst_expr(callee)),
                args: args.iter().map(|a| self.subst_expr(a)).collect(),
                ty: self.subst_type(ty),
            },
            tast::Expr::If {
                cond,
                then,
                orelse,
                ty,
            } => tast::Expr::If {
                cond: Box::new(self.subst_expr(cond)),
                then: Box::new(self.subst_expr(then)),
                orelse: Box::new(self.subst_expr(orelse)),
                ty: self.subst_type(ty),
            },
            tast::Expr::Unit { ty } => tast::Expr::Unit {
                ty: self.subst_type(ty),
            },
            tast::Expr::Tuple { exprs, ty } => tast::Expr::Tuple {
                exprs: exprs.iter().map(|e| self.subst_expr(e)).collect(),
                ty: self.subst_type(ty),
            },
            tast::Expr::Match { expr, arms, ty } => tast::Expr::Match {
                expr: Box::new(self.subst_expr(expr)),
                arms: arms
                    .iter()
                    .map(|(pat, expr)| (self.subst_pattern(pat), self.subst_expr(expr)))
                    .collect(),
                ty: self.subst_type(ty),
            },
        }
    }

    fn subst_stmt(&mut self, stmt: &tast::Stmt) -> tast::Stmt {
        match stmt {
            tast::Stmt::LetStmt { pat, ty, value } => tast::Stmt::LetStmt {
                pat: self.subst_pattern(pat),
                ty: self.subst_type(ty),
                value: self.subst_expr(value),
            },
            tast::Stmt::ExprStmt { expr } => tast::Stmt::ExprStmt {
                expr: self.subst_expr(expr),
            },
        }
    }

    fn subst_item(&mut self, item: &tast::Item) -> tast::Item {
        match item {
            tast::Item::Fn(tast::Fn {
                name,
                generics,
                params,
                ret_ty,
                body,
            }) => tast::Item::Fn(tast::Fn {
                name: name.clone(),
                generics: generics.clone(),
                params: params
                    .iter()
                    .map(|p| tast::Param {
                        name: p.name.clone(),
                        ty: self.subst_type(&p.ty),
                    })
                    .collect(),
                ret_ty: self.subst_type(ret_ty),
                body: Box::new(self.subst_expr(body)),
            }),
        }
    }

    pub fn subst_ast(&mut self, ast: tast::File) -> tast::File {
        tast::File {
            items: ast
                .items
                .into_iter()
                .map(|item| self.subst_item(&item))
                .collect(),
        }
    }
}

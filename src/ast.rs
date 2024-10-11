#![allow(unused)]

#[derive(Debug)]
pub struct File {
    pub items: Vec<Item>,
}

#[derive(Debug)]
pub enum Item {
    Fn(Fn),
    Enum(Enum),
}

#[derive(Debug)]
pub enum Type {
    Missing,
    Arrow { left: Box<Type>, right: Box<Type> },
    Tuple { items: Vec<Type> },
    Name { name: String },
    Inst { name: String, args: Vec<Type> },
}

#[derive(Debug)]
pub struct Fn {
    pub name: String,
    pub generics: Vec<String>,
    pub params: Vec<Param>,
    pub ret_ty: Type,
    pub body: Box<Expr>,
}

#[derive(Debug)]
pub struct Param {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug)]
pub struct Enum {
    pub name: String,
    pub generics: Vec<String>,
    pub variants: Vec<Variant>,
}

#[derive(Debug)]
pub struct Variant {
    pub name: String,
    pub args: Vec<Type>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Literal {
    Bool(bool),
    Int(i32),
    String(String),
}

#[derive(Debug)]
pub enum Stmt {
    LetStmt { pat: Pattern, ty: Type, value: Expr },
    ExprStmt { expr: Expr },
}

#[derive(Debug)]
pub enum Expr {
    Literal {
        value: Literal,
    },
    Name {
        name: String,
    },
    VCon {
        name: String,
    },
    Block {
        stmts: Vec<Stmt>,
        expr: Option<Box<Expr>>,
    },
    Call {
        f: Box<Expr>,
        args: Vec<Expr>,
    },
    If {
        cond: Box<Expr>,
        then: Box<Expr>,
        orelse: Box<Expr>,
    },
    Unit,
    Tuple {
        items: Vec<Expr>,
    },
    Match {
        expr: Box<Expr>,
        arms: Vec<(Pattern, Expr)>,
    },
}

#[derive(Debug)]
pub enum Pattern {
    Literal { pat: Literal },
    Tuple { pats: Vec<Pattern> },
    Ident { name: String },
    Constructor { name: String, args: Vec<Pattern> },
    Wild,
}

#[test]
fn x() {
    let x = Expr::Literal {
        value: Literal::Int(1),
    };
}

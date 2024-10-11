use ena::unify::{EqUnifyValue, UnifyKey};

#[derive(Clone, Eq, PartialEq)]
pub enum Builtin {
    Unit,
    Bool,
    Int,
    String,
}

impl std::fmt::Debug for Builtin {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Builtin::Unit => write!(f, "Unit"),
            Builtin::Bool => write!(f, "Bool"),
            Builtin::Int => write!(f, "Int"),
            Builtin::String => write!(f, "String"),
        }
    }
}

#[derive(Clone, Eq, PartialEq)]
pub enum Type {
    TVar {
        id: TypeVar,
    },
    Arrow {
        left: Vec<Type>,
        right: Box<Type>,
        generic: bool,
    },
    Param {
        index: u32,
        name: String,
    },
    Tuple {
        tys: Vec<Type>,
    },
    Builtin {
        ty: Builtin,
    },
    TConstr {
        tconstr: String,
        tys: Vec<Type>,
        generic: bool,
    },
}

impl EqUnifyValue for Type {}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Type::TVar { id } => write!(f, "{:?}", id),
            Type::Arrow {
                left,
                right,
                generic: _,
            } => {
                write!(f, "(")?;
                for (i, ty) in left.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{:?}", ty)?;
                }
                write!(f, ")")?;
                write!(f, " -> ")?;
                write!(f, "{:?}", right)
            }
            Type::Param { index, name } => {
                write!(f, "Param({}/{})", index, name)
            }
            Type::Tuple { tys } => {
                write!(f, "tuple(")?;
                for (i, ty) in tys.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{:?}", ty)?;
                }
                write!(f, ")")
            }
            Type::Builtin { ty } => write!(f, "{:?}", ty),
            Type::TConstr {
                tconstr: type_constructor,
                tys,
                generic: _,
            } => {
                write!(f, "{}", type_constructor)?;
                if !tys.is_empty() {
                    write!(f, "[")?;
                    for (i, ty) in tys.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{:?}", ty)?;
                    }
                    write!(f, "]")?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVar(pub u32);

impl UnifyKey for TypeVar {
    type Value = Option<Type>;

    fn index(&self) -> u32 {
        self.0
    }

    fn from_index(u: u32) -> TypeVar {
        TypeVar(u)
    }

    fn tag() -> &'static str {
        "TypeVar"
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct File {
    pub items: Vec<Item>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Item {
    Fn(Fn),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Fn {
    pub name: String,
    pub generics: Vec<String>,
    pub params: Vec<Param>,
    pub ret_ty: Type,
    pub body: Box<Expr>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Enum {
    pub name: String,
    pub generics: Vec<String>,
    pub variants: Vec<Variant>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Struct {
    pub name: String,
    pub generics: Vec<String>,
    pub fields: Vec<Field>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Variant {
    pub name: String,
    pub args: Option<Type>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Field {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Param {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Stmt {
    LetStmt { pat: Pattern, ty: Type, value: Expr },
    ExprStmt { expr: Expr },
}

impl Stmt {
    #[allow(unused)]
    pub fn get_type(&self) -> Type {
        match self {
            Stmt::LetStmt {
                pat: _,
                ty,
                value: _,
            } => ty.clone(),
            Stmt::ExprStmt { expr } => expr.get_type(),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ExprStmt {
    pub expr: Expr,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expr {
    Literal {
        value: Literal,
        ty: Type,
    },
    Name {
        name: String,
        ty: Type,
    },
    VCon {
        name: String,
        ty: Type,
    },
    Block {
        stmts: Vec<Stmt>,
        expr: Option<Box<Expr>>,
        ty: Type,
    },
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
        ty: Type,
    },
    If {
        cond: Box<Expr>,
        then: Box<Expr>,
        orelse: Box<Expr>,
        ty: Type,
    },
    Unit {
        ty: Type,
    },
    Tuple {
        exprs: Vec<Expr>,
        ty: Type,
    },
    Match {
        expr: Box<Expr>,
        arms: Vec<(Pattern, Expr)>,
        ty: Type,
    },
}

impl Expr {
    pub fn get_type(&self) -> Type {
        match self {
            Expr::Literal { value: _, ty } => ty.clone(),
            Expr::Name { name: _, ty } => ty.clone(),
            Expr::VCon { name: _, ty } => ty.clone(),
            Expr::Block {
                stmts: _,
                expr: _,
                ty,
            } => ty.clone(),
            Expr::Call {
                callee: _,
                args: _,
                ty,
            } => ty.clone(),
            Expr::If {
                cond: _,
                then: _,
                orelse: _,
                ty,
            } => ty.clone(),
            Expr::Unit { ty } => ty.clone(),
            Expr::Tuple { exprs: _, ty } => ty.clone(),
            Expr::Match {
                expr: _,
                arms: _,
                ty,
            } => ty.clone(),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct UnitExpr {}

#[derive(Clone, Eq, PartialEq)]
pub enum Literal {
    Bool(bool),
    Int(i32),
    String(String),
}

impl std::fmt::Debug for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Bool(b) => write!(f, "{}", b),
            Literal::Int(i) => write!(f, "{}", i),
            Literal::String(s) => write!(f, "{:?}", s),
        }
    }
}

impl Literal {
    pub fn get_type(&self) -> Type {
        match self {
            Literal::Bool(_) => Type::Builtin { ty: Builtin::Bool },
            Literal::Int(_) => Type::Builtin { ty: Builtin::Int },
            Literal::String(_) => Type::Builtin {
                ty: Builtin::String,
            },
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Pattern {
    Literal {
        pat: Literal,
        ty: Type,
    },
    Tuple {
        pats: Vec<Pattern>,
        ty: Type,
    },
    Ident {
        name: String,
        ty: Type,
    },
    Wild {
        ty: Type,
    },
    Constructor {
        name: String,
        args: Vec<Pattern>,
        ty: Type,
    },
}

impl Pattern {
    pub fn get_type(&self) -> Type {
        match self {
            Pattern::Literal { pat: _, ty } => ty.clone(),
            Pattern::Tuple { pats: _, ty } => ty.clone(),
            Pattern::Ident { name: _, ty } => ty.clone(),
            Pattern::Wild { ty } => ty.clone(),
            Pattern::Constructor {
                name: _,
                args: _,
                ty,
            } => ty.clone(),
        }
    }
}

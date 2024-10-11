use crate::tast;

pub const T_UNIT: tast::Type = tast::Type::Builtin {
    ty: tast::Builtin::Unit,
};
pub const T_BOOL: tast::Type = tast::Type::Builtin {
    ty: tast::Builtin::Bool,
};
pub const T_INT: tast::Type = tast::Type::Builtin {
    ty: tast::Builtin::Int,
};
pub const T_STRING: tast::Type = tast::Type::Builtin {
    ty: tast::Builtin::String,
};

pub fn create_init_type_env() -> im::HashMap<String, tast::Type> {
    let mut env: im::HashMap<String, tast::Type> = im::HashMap::new();
    env.insert(
        "print".to_string(),
        tast::Type::Arrow {
            left: vec![tast::Type::Param {
                index: 0,
                name: "T".to_string(),
            }],
            right: Box::new(tast::Type::Builtin {
                ty: tast::Builtin::Unit,
            }),
            generic: true,
        },
    );
    env.insert(
        "int_add".to_string(),
        tast::Type::Arrow {
            left: vec![
                tast::Type::Builtin {
                    ty: tast::Builtin::Int,
                },
                tast::Type::Builtin {
                    ty: tast::Builtin::Int,
                },
            ],
            right: Box::new(tast::Type::Builtin {
                ty: tast::Builtin::Int,
            }),
            generic: false,
        },
    );
    env.insert(
        "int_less".to_string(),
        tast::Type::Arrow {
            left: vec![
                tast::Type::Builtin {
                    ty: tast::Builtin::Int,
                },
                tast::Type::Builtin {
                    ty: tast::Builtin::Int,
                },
            ],
            right: Box::new(tast::Type::Builtin {
                ty: tast::Builtin::Bool,
            }),
            generic: false,
        },
    );
    env
}

pub fn create_init_kind_env() -> im::HashMap<String, tast::Type> {
    let mut kenv = im::HashMap::new();
    kenv.insert(
        "Unit".to_string(),
        tast::Type::Builtin {
            ty: tast::Builtin::Unit,
        },
    );
    kenv.insert(
        "Bool".to_string(),
        tast::Type::Builtin {
            ty: tast::Builtin::Bool,
        },
    );
    kenv.insert(
        "Int".to_string(),
        tast::Type::Builtin {
            ty: tast::Builtin::Int,
        },
    );
    kenv.insert(
        "String".to_string(),
        tast::Type::Builtin {
            ty: tast::Builtin::String,
        },
    );
    kenv
}

use crate::{grammar, typer};

#[test]
fn test_001() {
    let parser = grammar::StmtParser::new();
    let result = parser.parse("let _ : () = 1").unwrap();
    expect_test::expect![[r#"
        LetStmt {
            pat: Wild,
            ty: Tuple {
                items: [],
            },
            value: Literal {
                value: Int(
                    1,
                ),
            },
        }
    "#]]
    .assert_debug_eq(&result);
}

#[test]
fn test_002() {
    let parser = grammar::ExprParser::new();
    let result = parser.parse("{1; 2; 3; }").unwrap();
    expect_test::expect![[r#"
        Block {
            stmts: [
                ExprStmt {
                    expr: Literal {
                        value: Int(
                            1,
                        ),
                    },
                },
                ExprStmt {
                    expr: Literal {
                        value: Int(
                            2,
                        ),
                    },
                },
                ExprStmt {
                    expr: Literal {
                        value: Int(
                            3,
                        ),
                    },
                },
            ],
            expr: None,
        }
    "#]]
    .assert_debug_eq(&result);
}

#[test]
fn test_003() {
    let parser = grammar::FnParser::new();
    let result = parser.parse("fn id(x: Int) -> Int { x }").unwrap();
    expect_test::expect![[r#"
        Fn {
            name: "id",
            generics: [],
            params: [
                Param {
                    name: "x",
                    ty: Name {
                        name: "Int",
                    },
                },
            ],
            ret_ty: Name {
                name: "Int",
            },
            body: Block {
                stmts: [],
                expr: Some(
                    Name {
                        name: "x",
                    },
                ),
            },
        }
    "#]]
    .assert_debug_eq(&result);
}

#[test]
fn test_004() {
    let parser = grammar::ExprParser::new();
    let result = parser.parse("{ let x : Int = 1; x; }").unwrap();
    expect_test::expect![[r#"
        Block {
            stmts: [
                LetStmt {
                    pat: Ident {
                        name: "x",
                    },
                    ty: Name {
                        name: "Int",
                    },
                    value: Literal {
                        value: Int(
                            1,
                        ),
                    },
                },
                ExprStmt {
                    expr: Name {
                        name: "x",
                    },
                },
            ],
            expr: None,
        }
    "#]]
    .assert_debug_eq(&result);
}

#[test]
fn test_005() {
    let parser = grammar::ExprParser::new();
    let result = parser.parse("{ let x : Int = 1; x }").unwrap();
    expect_test::expect![[r#"
        Block {
            stmts: [
                LetStmt {
                    pat: Ident {
                        name: "x",
                    },
                    ty: Name {
                        name: "Int",
                    },
                    value: Literal {
                        value: Int(
                            1,
                        ),
                    },
                },
            ],
            expr: Some(
                Name {
                    name: "x",
                },
            ),
        }
    "#]]
    .assert_debug_eq(&result);
}

#[test]
fn test_006() {
    let parser = grammar::ExprParser::new();
    let result = parser.parse("{ let x : (Int, Int) = (1, 2); x }").unwrap();
    expect_test::expect![[r#"
        Block {
            stmts: [
                LetStmt {
                    pat: Ident {
                        name: "x",
                    },
                    ty: Tuple {
                        items: [
                            Name {
                                name: "Int",
                            },
                            Name {
                                name: "Int",
                            },
                        ],
                    },
                    value: Tuple {
                        items: [
                            Literal {
                                value: Int(
                                    1,
                                ),
                            },
                            Literal {
                                value: Int(
                                    2,
                                ),
                            },
                        ],
                    },
                },
            ],
            expr: Some(
                Name {
                    name: "x",
                },
            ),
        }
    "#]]
    .assert_debug_eq(&result);
}

#[test]
fn test_007() {
    let parser = grammar::ExprParser::new();
    let result = parser.parse("{ let x = (1, 2); x }").unwrap();
    expect_test::expect![[r#"
        Block {
            stmts: [
                LetStmt {
                    pat: Ident {
                        name: "x",
                    },
                    ty: Missing,
                    value: Tuple {
                        items: [
                            Literal {
                                value: Int(
                                    1,
                                ),
                            },
                            Literal {
                                value: Int(
                                    2,
                                ),
                            },
                        ],
                    },
                },
            ],
            expr: Some(
                Name {
                    name: "x",
                },
            ),
        }
    "#]]
    .assert_debug_eq(&result);
}

#[test]
fn test_008() {
    let parser = grammar::ExprParser::new();
    let result = parser.parse("f(1, 2, 3)").unwrap();
    expect_test::expect![[r#"
        Call {
            f: Name {
                name: "f",
            },
            args: [
                Literal {
                    value: Int(
                        1,
                    ),
                },
                Literal {
                    value: Int(
                        2,
                    ),
                },
                Literal {
                    value: Int(
                        3,
                    ),
                },
            ],
        }
    "#]]
    .assert_debug_eq(&result);
}

#[test]
fn test_009() {
    let parser = grammar::ExprParser::new();
    let result = parser.parse("if true { 1 } else { 2 }").unwrap();
    expect_test::expect![[r#"
        If {
            cond: Literal {
                value: Bool(
                    true,
                ),
            },
            then: Literal {
                value: Int(
                    1,
                ),
            },
            orelse: Literal {
                value: Int(
                    2,
                ),
            },
        }
    "#]]
    .assert_debug_eq(&result);
}

#[test]
fn test_010() {
    let parser = grammar::ExprParser::new();
    let result = parser.parse("if true { 1 } else { false }").unwrap();
    expect_test::expect![[r#"
        If {
            cond: Literal {
                value: Bool(
                    true,
                ),
            },
            then: Literal {
                value: Int(
                    1,
                ),
            },
            orelse: Literal {
                value: Bool(
                    false,
                ),
            },
        }
    "#]]
    .assert_debug_eq(&result);
}

#[test]
fn test_011() {
    let parser = grammar::ExprParser::new();
    let result = parser
        .parse(
            r#"
    match x {
        0 => 1,
        1 => {
          let x = 1;
          x
        },
        _ => 2
    }
    "#,
        )
        .unwrap();
    expect_test::expect![[r#"
        Match {
            expr: Name {
                name: "x",
            },
            arms: [
                (
                    Literal {
                        pat: Int(
                            0,
                        ),
                    },
                    Literal {
                        value: Int(
                            1,
                        ),
                    },
                ),
                (
                    Literal {
                        pat: Int(
                            1,
                        ),
                    },
                    Block {
                        stmts: [
                            LetStmt {
                                pat: Ident {
                                    name: "x",
                                },
                                ty: Missing,
                                value: Literal {
                                    value: Int(
                                        1,
                                    ),
                                },
                            },
                        ],
                        expr: Some(
                            Name {
                                name: "x",
                            },
                        ),
                    },
                ),
                (
                    Wild,
                    Literal {
                        value: Int(
                            2,
                        ),
                    },
                ),
            ],
        }
    "#]]
    .assert_debug_eq(&result);
}

#[test]
fn test_012() {
    let parser = grammar::FileParser::new();
    let result = parser
        .parse(
            r#"
    fn int_id(x: Int) -> Int {
        x
    }

    fn int_add(x: Int, y: Int) -> Int {
        add(x, y)
    }
    

    "#,
        )
        .unwrap();
    expect_test::expect![[r#"
        File {
            items: [
                Fn(
                    Fn {
                        name: "int_id",
                        generics: [],
                        params: [
                            Param {
                                name: "x",
                                ty: Name {
                                    name: "Int",
                                },
                            },
                        ],
                        ret_ty: Name {
                            name: "Int",
                        },
                        body: Block {
                            stmts: [],
                            expr: Some(
                                Name {
                                    name: "x",
                                },
                            ),
                        },
                    },
                ),
                Fn(
                    Fn {
                        name: "int_add",
                        generics: [],
                        params: [
                            Param {
                                name: "x",
                                ty: Name {
                                    name: "Int",
                                },
                            },
                            Param {
                                name: "y",
                                ty: Name {
                                    name: "Int",
                                },
                            },
                        ],
                        ret_ty: Name {
                            name: "Int",
                        },
                        body: Block {
                            stmts: [],
                            expr: Some(
                                Call {
                                    f: Name {
                                        name: "add",
                                    },
                                    args: [
                                        Name {
                                            name: "x",
                                        },
                                        Name {
                                            name: "y",
                                        },
                                    ],
                                },
                            ),
                        },
                    },
                ),
            ],
        }
    "#]]
    .assert_debug_eq(&result);
}

#[test]
fn test_013() {
    let parser = grammar::FileParser::new();
    let result = parser
        .parse(
            r#"
    enum Color {
        Red,
        Green,
        Blue,
    }

    fn color_to_int(c: Color) -> Int {
        match c {
            Red => 0,
            Green => 1,
            Blue => 2,
        }
    }

    "#,
        )
        .unwrap();
    expect_test::expect![[r#"
        File {
            items: [
                Enum(
                    Enum {
                        name: "Color",
                        generics: [],
                        variants: [
                            Variant {
                                name: "Red",
                                args: [],
                            },
                            Variant {
                                name: "Green",
                                args: [],
                            },
                            Variant {
                                name: "Blue",
                                args: [],
                            },
                        ],
                    },
                ),
                Fn(
                    Fn {
                        name: "color_to_int",
                        generics: [],
                        params: [
                            Param {
                                name: "c",
                                ty: Name {
                                    name: "Color",
                                },
                            },
                        ],
                        ret_ty: Name {
                            name: "Int",
                        },
                        body: Block {
                            stmts: [],
                            expr: Some(
                                Match {
                                    expr: Name {
                                        name: "c",
                                    },
                                    arms: [
                                        (
                                            Constructor {
                                                name: "Red",
                                                args: [],
                                            },
                                            Literal {
                                                value: Int(
                                                    0,
                                                ),
                                            },
                                        ),
                                        (
                                            Constructor {
                                                name: "Green",
                                                args: [],
                                            },
                                            Literal {
                                                value: Int(
                                                    1,
                                                ),
                                            },
                                        ),
                                        (
                                            Constructor {
                                                name: "Blue",
                                                args: [],
                                            },
                                            Literal {
                                                value: Int(
                                                    2,
                                                ),
                                            },
                                        ),
                                    ],
                                },
                            ),
                        },
                    },
                ),
            ],
        }
    "#]]
    .assert_debug_eq(&result);
}

#[test]
fn test_014() {
    let parser = grammar::FileParser::new();
    let result = parser
        .parse(
            r#"
    enum IntList {
        Nil,
        Cons(Int, IntList)
    }

    fn sum(xs: IntList) -> Int {
        match xs {
            Nil => 0,
            Cons(x, xs) => add(x, sum(xs)),
        }
    }

    "#,
        )
        .unwrap();
    expect_test::expect![[r#"
        File {
            items: [
                Enum(
                    Enum {
                        name: "IntList",
                        generics: [],
                        variants: [
                            Variant {
                                name: "Nil",
                                args: [],
                            },
                            Variant {
                                name: "Cons",
                                args: [
                                    Name {
                                        name: "Int",
                                    },
                                    Name {
                                        name: "IntList",
                                    },
                                ],
                            },
                        ],
                    },
                ),
                Fn(
                    Fn {
                        name: "sum",
                        generics: [],
                        params: [
                            Param {
                                name: "xs",
                                ty: Name {
                                    name: "IntList",
                                },
                            },
                        ],
                        ret_ty: Name {
                            name: "Int",
                        },
                        body: Block {
                            stmts: [],
                            expr: Some(
                                Match {
                                    expr: Name {
                                        name: "xs",
                                    },
                                    arms: [
                                        (
                                            Constructor {
                                                name: "Nil",
                                                args: [],
                                            },
                                            Literal {
                                                value: Int(
                                                    0,
                                                ),
                                            },
                                        ),
                                        (
                                            Constructor {
                                                name: "Cons",
                                                args: [
                                                    Ident {
                                                        name: "x",
                                                    },
                                                    Ident {
                                                        name: "xs",
                                                    },
                                                ],
                                            },
                                            Call {
                                                f: Name {
                                                    name: "add",
                                                },
                                                args: [
                                                    Name {
                                                        name: "x",
                                                    },
                                                    Call {
                                                        f: Name {
                                                            name: "sum",
                                                        },
                                                        args: [
                                                            Name {
                                                                name: "xs",
                                                            },
                                                        ],
                                                    },
                                                ],
                                            },
                                        ),
                                    ],
                                },
                            ),
                        },
                    },
                ),
            ],
        }
    "#]]
    .assert_debug_eq(&result);
}

#[test]
fn test_015() {
    let parser = grammar::FileParser::new();
    let result = parser
        .parse(
            r#"
    enum List[T] {
        Nil,
        Cons(T, List[T])
    }

    fn id[T](x: T) -> T {
        x
    }

    "#,
        )
        .unwrap();
    expect_test::expect![[r#"
        File {
            items: [
                Enum(
                    Enum {
                        name: "List",
                        generics: [
                            "T",
                        ],
                        variants: [
                            Variant {
                                name: "Nil",
                                args: [],
                            },
                            Variant {
                                name: "Cons",
                                args: [
                                    Name {
                                        name: "T",
                                    },
                                    Inst {
                                        name: "List",
                                        args: [
                                            Name {
                                                name: "T",
                                            },
                                        ],
                                    },
                                ],
                            },
                        ],
                    },
                ),
                Fn(
                    Fn {
                        name: "id",
                        generics: [
                            "T",
                        ],
                        params: [
                            Param {
                                name: "x",
                                ty: Name {
                                    name: "T",
                                },
                            },
                        ],
                        ret_ty: Name {
                            name: "T",
                        },
                        body: Block {
                            stmts: [],
                            expr: Some(
                                Name {
                                    name: "x",
                                },
                            ),
                        },
                    },
                ),
            ],
        }
    "#]]
    .assert_debug_eq(&result);
}

#[test]
fn test_016() {
    let parser = grammar::FileParser::new();
    let result = parser
        .parse(
            r#"
    fn add(x: Int, y: Int) -> Int { 0 }
    fn main() -> Unit {
        let x = 1;
        let y = 2;
        let z = add(x, y);
    }
    "#,
        )
        .unwrap();
    let mut typer = typer::Typer::new();
    let tast = typer.infer(&result);
    expect_test::expect![[r#"
        File {
            items: [
                Fn(
                    Fn {
                        name: "add",
                        generics: [],
                        params: [
                            Param {
                                name: "x",
                                ty: Int,
                            },
                            Param {
                                name: "y",
                                ty: Int,
                            },
                        ],
                        ret_ty: Int,
                        body: Block {
                            stmts: [],
                            expr: Some(
                                Literal {
                                    value: 0,
                                    ty: Int,
                                },
                            ),
                            ty: Int,
                        },
                    },
                ),
                Fn(
                    Fn {
                        name: "main",
                        generics: [],
                        params: [],
                        ret_ty: Unit,
                        body: Block {
                            stmts: [
                                LetStmt {
                                    pat: Ident {
                                        name: "x",
                                        ty: Int,
                                    },
                                    ty: Int,
                                    value: Literal {
                                        value: 1,
                                        ty: Int,
                                    },
                                },
                                LetStmt {
                                    pat: Ident {
                                        name: "y",
                                        ty: Int,
                                    },
                                    ty: Int,
                                    value: Literal {
                                        value: 2,
                                        ty: Int,
                                    },
                                },
                                LetStmt {
                                    pat: Ident {
                                        name: "z",
                                        ty: Int,
                                    },
                                    ty: Int,
                                    value: Call {
                                        callee: Name {
                                            name: "add",
                                            ty: (Int, Int) -> Int,
                                        },
                                        args: [
                                            Name {
                                                name: "x",
                                                ty: Int,
                                            },
                                            Name {
                                                name: "y",
                                                ty: Int,
                                            },
                                        ],
                                        ty: Int,
                                    },
                                },
                            ],
                            expr: None,
                            ty: Unit,
                        },
                    },
                ),
            ],
        }
    "#]]
    .assert_debug_eq(&tast);
}

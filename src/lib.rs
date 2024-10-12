pub mod ast;
pub mod tast;
pub mod typer;

lalrpop_util::lalrpop_mod!(
    #[allow(clippy::ptr_arg)]
    #[rustfmt::skip]
    pub grammar
);

pub mod builtin;
pub mod constraint;
pub mod env;
pub mod infer;
pub mod subst;
pub mod toplevel;
pub mod tx;
pub mod unification;
pub mod util;

use ena::unify::InPlaceUnificationTable;
use env::KindEnv;
use env::TypeEnv;
use unification::TypeError;

use crate::tast::TypeVar;

#[derive(Debug)]
pub struct Typer {
    pub unification_table: InPlaceUnificationTable<TypeVar>,
    pub errors: Vec<TypeError>,
    kind_env: Vec<KindEnv>,
    type_env: Vec<TypeEnv>,
}

impl Default for Typer {
    fn default() -> Self {
        Self::new()
    }
}

impl Typer {
    pub fn new() -> Self {
        let unification_table = InPlaceUnificationTable::new();
        let type_env = vec![TypeEnv::new_with_builtin()];
        let kind_env = vec![KindEnv::new_with_builtin()];

        let errors = vec![];
        Typer {
            unification_table,
            errors,
            kind_env,
            type_env,
        }
    }
}

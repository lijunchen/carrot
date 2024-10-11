use crate::tast;

#[derive(Debug, Clone)]
pub enum Constraint {
    TypeEqual(tast::Type, tast::Type),
}

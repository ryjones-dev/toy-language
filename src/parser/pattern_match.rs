use super::{literal::Literal, variable::Variable};

#[derive(Debug, Clone)]
pub(crate) enum Pattern {
    IntLiteral(Literal<i64>),
    BoolLiteral(Literal<bool>),
    Variable(Variable),
}

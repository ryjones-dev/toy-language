use super::source_range::SourceRange;

/// A distinct type that is used to represent integer literal values.
///
/// It contains extra information about where in the source code it is located,
/// which is needed for helpful error messages.
#[derive(Debug, Clone, Copy)]
pub(crate) struct IntLiteral {
    val: i64,
    pub(crate) source: SourceRange,
}

impl IntLiteral {
    pub(super) fn new(val: i64, source: SourceRange) -> Self {
        Self { val, source }
    }
}

impl From<IntLiteral> for i64 {
    fn from(value: IntLiteral) -> Self {
        value.val
    }
}

/// A distinct type that is used to represent boolean literal values.
///
/// It contains extra information about where in the source code it is located,
/// which is needed for helpful error messages.
#[derive(Debug, Clone, Copy)]
pub(crate) struct BoolLiteral {
    val: bool,
    pub(crate) source: SourceRange,
}

impl BoolLiteral {
    pub(super) fn new(val: bool, source: SourceRange) -> Self {
        Self { val, source }
    }
}

impl From<BoolLiteral> for bool {
    fn from(value: BoolLiteral) -> Self {
        value.val
    }
}

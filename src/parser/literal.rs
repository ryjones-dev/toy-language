use super::source_range::SourceRange;

#[derive(Debug, Clone, Copy, Eq)]
pub(crate) struct Literal<T: Copy + PartialEq> {
    val: T,
    source: SourceRange,
}

impl<T: Copy + PartialEq> Literal<T> {
    pub(super) fn new(val: T, source: SourceRange) -> Self {
        Self { val, source }
    }

    pub(crate) fn val(&self) -> T {
        self.val
    }

    pub(crate) fn source(&self) -> SourceRange {
        self.source
    }
}

// The source range should not affect equivalence or hashing
impl<T: Copy + PartialEq> PartialEq for Literal<T> {
    fn eq(&self, other: &Self) -> bool {
        self.val == other.val
    }
}

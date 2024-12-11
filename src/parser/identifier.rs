use super::source_range::SourceRange;

/// A distinct type that is used to represent names of particular code primitives, such as functions and variables.
///
/// This is just a [`String`] with extra information about where in the source code it is located,
/// which is needed for helpful error messages.
///
/// Identifiers can be discarded by prefixing the name with an underscore. This means that the variable
/// is intentionally not used. If the variable is discarded but then later used, this results in a compile error.
#[derive(Debug, Clone, Eq)]
pub(crate) struct Identifier {
    val: String,
    source: SourceRange,
}

impl Identifier {
    pub(super) fn new(val: String, source: SourceRange) -> Self {
        Self { val, source }
    }

    pub(crate) fn source(&self) -> SourceRange {
        self.source
    }

    pub(super) fn is_discarded(&self) -> bool {
        self.val.starts_with("_")
    }
}

impl<'a> From<&'a Identifier> for &'a str {
    fn from(value: &'a Identifier) -> Self {
        &value.val
    }
}

// The source range should not affect equivalence or hashing
impl PartialEq for Identifier {
    fn eq(&self, other: &Self) -> bool {
        self.val == other.val
    }
}

impl std::hash::Hash for Identifier {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.val.hash(state);
    }
}

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.val)
    }
}

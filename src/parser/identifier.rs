use super::source_range::SourceRange;

/// A distinct type that is used to represent names of functions and variables.
///
/// It contains extra information about where in the source code it is located,
/// which is needed for helpful error messages.
///
/// While there are contexts where using the discard identifier (`"_"`) makes sense, it is not universal,
/// so instead of building it in to the type, the discard identifier is individually considered in the contexts
/// it make sense.
#[derive(Debug, Clone, Eq)]
pub struct Identifier {
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

impl From<Identifier> for String {
    fn from(value: Identifier) -> Self {
        value.val
    }
}

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.val)
    }
}

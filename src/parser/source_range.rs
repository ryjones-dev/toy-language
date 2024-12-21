/// A helper type that keeps track of a region of source code.
///
/// Other AST types will contain one of these so that they can keep track
/// of which part of the source code they represent.
/// This is needed for emiting helpful error messages.
///
/// [`SourceRange`] is convertible to and from [`std::ops::RangeInclusive<usize>`].
/// To construct a [`SourceRange`], convert from a [`std::ops::RangeInclusive<usize>`] (`(1..=3).into()`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct SourceRange {
    start: usize,
    end: usize,
}

impl SourceRange {
    /// Combines two source ranges, returning a new [`SourceRange`] that starts at the start
    /// of this range and ends at the end of the other range.
    pub(crate) fn combine(self, other: Self) -> Self {
        assert!(self.start <= other.end);

        Self {
            start: self.start,
            end: other.end,
        }
    }
}

impl From<SourceRange> for std::ops::RangeInclusive<usize> {
    fn from(value: SourceRange) -> Self {
        value.start..=value.end
    }
}

impl From<std::ops::RangeInclusive<usize>> for SourceRange {
    fn from(value: std::ops::RangeInclusive<usize>) -> Self {
        Self {
            start: *value.start(),
            end: *value.end(),
        }
    }
}

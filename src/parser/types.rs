use thiserror::Error;

use super::source_range::SourceRange;

#[derive(Debug, Error)]
#[error("\"{0}\" is not a valid type")]
pub struct ParseTypeError(String);

/// Represents TODO_LANG_NAME built-in data types.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum DataType {
    Int,
    Bool,
}

impl std::str::FromStr for DataType {
    type Err = ParseTypeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "int" => Ok(DataType::Int),
            "bool" => Ok(DataType::Bool),
            _ => Err(ParseTypeError(s.to_string())),
        }
    }
}

impl std::fmt::Display for DataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DataType::Int => write!(f, "int"),
            DataType::Bool => write!(f, "bool"),
        }
    }
}

/// A TODO_LANG_NAME type parsed from source code.
#[derive(Debug, Clone, Copy)]
pub(crate) struct Type {
    pub(crate) ty: DataType,
    pub(crate) source: SourceRange,
}

impl Type {
    pub(crate) fn new(ty: DataType, source: SourceRange) -> Self {
        Self { ty, source }
    }
}

// The source range should not affect equivalence or hashing
impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        self.ty == other.ty
    }
}

impl std::hash::Hash for Type {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.ty.hash(state);
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ty)
    }
}

/// A list of related TODO_LANG_NAME types.
///
/// Wrapping the list helpful to display better output in error messages when listing types.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Types(Vec<Type>);

impl Types {
    pub(crate) fn new() -> Self {
        Self(Vec::new())
    }

    /// Returns a [`SourceRange`] from the beginning of the type list to the end.
    /// Returns [`None`] if the type list is empty.
    pub(crate) fn source(&self) -> Option<SourceRange> {
        if self.len() > 0 {
            Some(
                self.first()
                    .unwrap()
                    .source
                    .combine(self.last().unwrap().source),
            )
        } else {
            None
        }
    }
}

impl From<Vec<Type>> for Types {
    fn from(value: Vec<Type>) -> Self {
        Self(value)
    }
}

impl FromIterator<Type> for Types {
    fn from_iter<T: IntoIterator<Item = Type>>(iter: T) -> Self {
        Self(Vec::from_iter(iter))
    }
}

impl<'a> IntoIterator for &'a Types {
    type Item = &'a Type;
    type IntoIter = std::slice::Iter<'a, Type>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl std::ops::Deref for Types {
    type Target = Vec<Type>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for Types {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl std::fmt::Display for Types {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "`")?;

        if self.len() == 0 {
            write!(f, "()")?;
        }

        for (i, ty) in self.0.iter().enumerate() {
            write!(f, "{}", ty)?;

            if i < self.0.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, "`")?;

        Ok(())
    }
}

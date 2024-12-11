use super::{source_range::SourceRange, Struct};

/// Represents TODO_LANG_NAME data types.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum DataType {
    Int,
    Float,
    Bool,
    Struct(String),
}

impl From<&str> for DataType {
    fn from(value: &str) -> Self {
        match value {
            "int" => DataType::Int,
            "float" => DataType::Float,
            "bool" => DataType::Bool,
            name => DataType::Struct(name.to_owned()),
        }
    }
}

impl std::fmt::Display for DataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DataType::Int => write!(f, "int"),
            DataType::Float => write!(f, "float"),
            DataType::Bool => write!(f, "bool"),
            DataType::Struct(name) => write!(f, "{}", name),
        }
    }
}

/// A TODO_LANG_NAME type parsed from source code.
#[derive(Debug, Clone, Eq)]
pub(crate) struct Type {
    ty: DataType,
    source: SourceRange,

    /// This is only a [`Some`] value if this type has a [`DataType::Struct`] data type.
    /// This will have a copy of the struct this type refers to, which helps with codegen.
    _struct: Option<Struct>,
}

impl Type {
    pub(crate) fn new(ty: DataType, source: SourceRange) -> Self {
        Self {
            ty,
            source,
            _struct: None,
        }
    }

    /// Sets the struct this type refers to.
    ///
    /// Can only be used if this type has a [`DataType::Struct`] type.
    pub(crate) fn set_struct(&mut self, _struct: Struct) {
        debug_assert!(
            self.ty == DataType::Struct(_struct.name().to_string()),
            "attempted to assign struct data to a non-struct type"
        );

        self._struct = Some(_struct)
    }

    pub(crate) fn source(&self) -> SourceRange {
        self.source
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

/// Helpers for comparing a [`Type`] and [`DataType`] directly.
impl PartialEq<DataType> for Type {
    fn eq(&self, other: &DataType) -> bool {
        self.ty == *other
    }
}

impl PartialEq<Type> for DataType {
    fn eq(&self, other: &Type) -> bool {
        *self == other.ty
    }
}

/// Helper for converting a [`Type`] into a [`DataType`].
impl From<Type> for DataType {
    fn from(value: Type) -> Self {
        value.ty
    }
}

/// Helper for converting a [`Type`] into a [`DataType`].
impl<'a> From<&'a Type> for &'a DataType {
    fn from(value: &'a Type) -> Self {
        &value.ty
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ty)
    }
}

/// A list of related TODO_LANG_NAME types.
///
/// Wrapping the list is needed to display better output in error messages when listing types.
#[derive(Debug, Default, Clone, PartialEq)]
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

use super::source_range::SourceRange;

/// Represents TODO_LANG_NAME data types.
#[derive(Debug, Clone, Eq)]
pub(crate) enum DataType {
    Int,
    Float,
    Bool,

    /// Struct data types contain a list of other data types that make up the [`Struct`]
    /// that it refers to. This list cannot be parsed from the source code,
    /// so it will be [`None`] until it is populated by semantic analysis.
    Struct {
        name: String,
        struct_data_types: Option<Vec<DataType>>,
    },
}

// The optional struct copy should not affect equivalence or hashing
impl PartialEq for DataType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (DataType::Int, DataType::Int) => true,
            (DataType::Float, DataType::Float) => true,
            (DataType::Bool, DataType::Bool) => true,
            (
                DataType::Struct {
                    name: self_name, ..
                },
                DataType::Struct {
                    name: other_name, ..
                },
            ) => *self_name == *other_name,
            _ => false,
        }
    }
}

impl std::hash::Hash for DataType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            DataType::Int => {
                state.write_u8(1);
            }
            DataType::Float => {
                state.write_u8(2);
            }
            DataType::Bool => {
                state.write_u8(3);
            }
            DataType::Struct { name, .. } => {
                state.write_u8(4);
                name.hash(state);
            }
        }
    }
}

impl From<&str> for DataType {
    fn from(value: &str) -> Self {
        match value {
            "int" => DataType::Int,
            "float" => DataType::Float,
            "bool" => DataType::Bool,
            name => DataType::Struct {
                name: name.to_owned(),
                struct_data_types: None,
            },
        }
    }
}

impl std::fmt::Display for DataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DataType::Int => write!(f, "int"),
            DataType::Float => write!(f, "float"),
            DataType::Bool => write!(f, "bool"),
            DataType::Struct { name, .. } => write!(f, "{}", name),
        }
    }
}

/// A TODO_LANG_NAME type parsed from source code.
#[derive(Debug, Clone, Eq)]
pub(crate) struct Type {
    ty: DataType,
    source: SourceRange,
}

impl Type {
    pub(crate) fn new(ty: DataType, source: SourceRange) -> Self {
        Self { ty, source }
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

/// Helper for converting a [`Type`] into a [`DataType`].
impl<'a> From<&'a mut Type> for &'a mut DataType {
    fn from(value: &'a mut Type) -> Self {
        &mut value.ty
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

impl IntoIterator for Types {
    type Item = Type;
    type IntoIter = std::vec::IntoIter<Type>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
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

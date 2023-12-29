use super::{
    function::FunctionParameter,
    identifier::Identifier,
    source_range::SourceRange,
    types::{DataType, Type},
};

/// A distinct type that is used to represent a variable.
///
/// Only the name is parsable from the source code. The type will be [`None`]
/// until semantic analysis can determine the type.
#[derive(Debug, Clone)]
pub(crate) struct Variable {
    pub(crate) name: Identifier,
    pub(crate) ty: Option<DataType>,
}

impl Variable {
    pub(crate) fn new(name: Identifier) -> Self {
        Self { name, ty: None }
    }

    /// Converts a [`Variable`] to a [`FunctionParameter`].
    ///
    /// # Panics
    /// Panics if the [`Variable`] does not have a [`DataType`] set.
    pub(crate) fn to_param(&self) -> FunctionParameter {
        FunctionParameter {
            name: self.name.clone(),
            ty: Type::new(
                self.ty.expect("variable does not have a data type"),
                self.name.source(),
            ),
        }
    }

    pub(crate) fn is_discarded(&self) -> bool {
        self.name.is_discarded()
    }
}

/// Built in conversion for turning function parameters into variables
impl From<FunctionParameter> for Variable {
    fn from(value: FunctionParameter) -> Self {
        Self {
            name: value.name,
            ty: Some(value.ty.into()),
        }
    }
}

impl std::fmt::Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

/// A list of related TODO_LANG_NAME variables.
///
/// Wrapping the list is convenient for getting the [`SourceRange`] of the variable list.
#[derive(Debug, Clone)]
pub(crate) struct Variables(Vec<Variable>);

impl Variables {
    /// Returns a [`SourceRange`] from the beginning of the variable list to the end.
    /// Returns [`None`] if the variable list is empty.
    pub(crate) fn source(&self) -> Option<SourceRange> {
        if self.len() > 0 {
            Some(
                self.first()
                    .unwrap()
                    .name
                    .source()
                    .combine(self.last().unwrap().name.source()),
            )
        } else {
            None
        }
    }
}

impl FromIterator<Variable> for Variables {
    fn from_iter<T: IntoIterator<Item = Variable>>(iter: T) -> Self {
        Self(Vec::from_iter(iter))
    }
}

impl IntoIterator for Variables {
    type Item = Variable;
    type IntoIter = std::vec::IntoIter<Variable>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl std::ops::Deref for Variables {
    type Target = Vec<Variable>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for Variables {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

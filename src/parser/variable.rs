use super::{function::FunctionParameter, identifier::Identifier, types::DataType};

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
}

/// Built in conversion for turning function parameters into variables
impl From<FunctionParameter> for Variable {
    fn from(value: FunctionParameter) -> Self {
        Self {
            name: value.name,
            ty: Some(value.ty.ty),
        }
    }
}

impl std::fmt::Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

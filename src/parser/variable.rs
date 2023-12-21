use super::{identifier::Identifier, types::Type};

/// A distinct type that is used to represent a variable.
///
/// Only the name is parsable from the source code. The type will be [`Option::None`]
/// until semantic analysis can determine the type.
#[derive(Debug, Clone)]
pub(crate) struct Variable {
    pub(crate) name: Identifier,
    pub(crate) ty: Option<Type>,
}

impl Variable {
    pub(crate) fn new(name: Identifier) -> Self {
        Self { name, ty: None }
    }

    pub(super) fn with_type(self, ty: Type) -> Self {
        Self {
            name: self.name,
            ty: Some(ty),
        }
    }
}

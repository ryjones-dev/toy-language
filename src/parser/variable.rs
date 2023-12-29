use crate::semantic::EXPECT_VAR_TYPE;

use super::{
    function::FunctionParameter, identifier::Identifier, source_range::SourceRange, types::Type,
};

/// A distinct type that is used to represent a variable.
///
/// Only the name is parsable from the source code. The type will be [`None`]
/// until semantic analysis can determine the type.
#[derive(Debug, Clone)]
pub(crate) struct Variable {
    name: Identifier,
    ty: Option<Type>,
}

impl Variable {
    pub(crate) fn new(name: Identifier) -> Self {
        Self { name, ty: None }
    }

    pub(crate) fn name(&self) -> &Identifier {
        &self.name
    }

    pub(crate) fn into_name(self) -> Identifier {
        self.name
    }

    pub(crate) fn get_type(&self) -> &Option<Type> {
        &self.ty
    }

    pub(crate) fn set_type(&mut self, ty: &Type) {
        debug_assert!(self.ty.is_none(), "attempted to reassign variable type");

        // Keep the variable's name source for its type
        self.ty = Some(Type::new((*ty).into(), self.name.source()))
    }

    /// Returns a [`SourceRange`] from the variable name to the variable's explicit type annotation, if present.
    /// If not present, it will just be the variable's name.
    pub(crate) fn source(&self) -> SourceRange {
        if let Some(ty) = self.ty {
            self.name.source().combine(ty.source())
        } else {
            self.name.source()
        }
    }

    /// Converts a [`Variable`] to a [`FunctionParameter`].
    ///
    /// # Panics
    /// Panics if the [`Variable`] does not have a [`Type`] set.
    pub(crate) fn to_param(&self) -> FunctionParameter {
        FunctionParameter {
            name: self.name.clone(),
            ty: self.ty.expect(EXPECT_VAR_TYPE),
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
                    .source()
                    .combine(self.last().unwrap().source()),
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

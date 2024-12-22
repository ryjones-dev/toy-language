use crate::{parser::types::DataType, semantic::EXPECT_VAR_TYPE};

use super::{
    identifier::Identifier,
    source_range::SourceRange,
    types::{Type, Types},
    Struct,
};

/// A distinct type that is used to represent a variable.
///
/// Only the name is parsable from the source code. The type will be [`None`]
/// until semantic analysis can determine the type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Variable {
    name: Identifier,
    ty: Option<Type>,
}

impl Variable {
    pub(super) fn new(name: Identifier, ty: Option<Type>) -> Self {
        Self { name, ty }
    }

    pub(crate) fn name(&self) -> &Identifier {
        &self.name
    }

    pub(crate) fn get_type(&self) -> &Option<Type> {
        &self.ty
    }

    pub(crate) fn set_type(&mut self, ty: &Type) {
        assert!(self.ty.is_none(), "attempted to reassign variable type");

        // Keep the variable's name source for its type
        self.ty = Some(Type::new(ty.clone().into(), self.name.source()))
    }

    pub(crate) fn update_struct_data_type(&mut self, _struct: Struct) {
        match &mut self.ty {
            Some(ty) => match ty.into() {
                &mut DataType::Int | &mut DataType::Float | &mut DataType::Bool => {
                    panic!("attempted to update struct data of variable with non-struct type")
                }
                &mut DataType::Struct {
                    ref mut struct_data_types,
                    ..
                } => match struct_data_types {
                    Some(_) => panic!(
                        "attempted to update struct data of variable with existing struct data"
                    ),
                    None => {
                        *struct_data_types = Some(
                            _struct
                                .into_members()
                                .into_iter()
                                .map(|member| member.into_type().into())
                                .collect(),
                        )
                    }
                },
            },
            None => panic!("attempted to update struct data of variable with no type set"),
        }
    }

    pub(crate) fn into_type(self) -> Option<Type> {
        self.ty
    }

    /// Returns a [`SourceRange`] from the variable name to the variable's explicit type annotation, if present.
    /// If not present, it will just be the variable's name.
    pub(crate) fn source(&self) -> SourceRange {
        if let Some(ty) = &self.ty {
            self.name.source().combine(ty.source())
        } else {
            self.name.source()
        }
    }

    pub(crate) fn is_discarded(&self) -> bool {
        self.name.is_discarded()
    }
}

impl std::fmt::Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

/// A list of related TODO_LANG_NAME variables parsed from source code.
///
/// Wrapping the list is convenient for getting the [`SourceRange`] of the variable list.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Variables(Vec<Variable>);

impl Variables {
    /// Returns the type of each [`Variable`] in [`Types`].
    ///
    /// # Panics
    /// Panics if any [`Variable`]'s type is [`None`].
    pub(crate) fn types(&self) -> Types {
        self.iter()
            .map(|var| var.ty.clone().expect(EXPECT_VAR_TYPE))
            .collect()
    }

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

impl<'a> IntoIterator for &'a Variables {
    type Item = &'a Variable;
    type IntoIter = std::slice::Iter<'a, Variable>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
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

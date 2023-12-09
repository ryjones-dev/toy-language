use std::collections::HashMap;

use crate::ast_parser::types::Identifier;

/// Represents a reference to a defined variable.
///
/// Currently this just wraps Cranelift's [`cranelift::frontend::Variable`]
/// and is completely convertable to and from [`cranelift::frontend::Variable`].
/// This hides the Cranelift API from the user.
#[derive(Copy, Clone, PartialEq, Eq)]
pub(super) struct Variable(cranelift::frontend::Variable);

impl From<Variable> for cranelift::frontend::Variable {
    fn from(value: Variable) -> Self {
        value.0
    }
}

impl From<cranelift::frontend::Variable> for Variable {
    fn from(value: cranelift::frontend::Variable) -> Self {
        Self(value)
    }
}

/// Keeps track of what variables have been declared.
///
/// This is a helper struct that keeps track of internal indexing logic that is specific to Cranelift.
/// [`VariableMap`] has no concept of scope, so it is up to the user
/// to create and drop as necessary to enforce scoping rules.
#[derive(Default)]
pub(super) struct VariableMap {
    map: HashMap<Identifier, Variable>,
    index: u32,
}

impl VariableMap {
    pub(super) fn new() -> Self {
        Default::default()
    }

    pub(super) fn get(&self, name: &Identifier) -> Option<&Variable> {
        self.map.get(name)
    }

    pub(super) fn insert(&mut self, name: &Identifier) -> &Variable {
        // TODO: how should shadowing be handled? Explicit or allowed? Allow for now.
        self.map.insert(
            name.clone(),
            Variable(cranelift::frontend::Variable::from_u32(self.index)),
        );
        self.index += 1;

        self.map.get(&name).unwrap()
    }
}

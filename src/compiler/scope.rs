use std::collections::HashMap;

use crate::ast_parser::types::Identifier;

/// Represents a reference to a defined variable.
///
/// Currently this just wraps Cranelift's [`cranelift::frontend::Variable`]
/// and is completely convertable to and from [`cranelift::frontend::Variable`].
/// This hides the Cranelift API from the user.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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
/// [`VariableMap`] has no concept of scope, that is covered by [`Scope`].
#[derive(Debug, Default)]
struct VariableMap {
    map: HashMap<Identifier, Variable>,
    index: u32,
}

impl VariableMap {
    fn new() -> Self {
        Default::default()
    }

    fn get(&self, name: &Identifier) -> Option<&Variable> {
        self.map.get(name)
    }

    fn insert(&mut self, name: &Identifier) -> &Variable {
        // TODO: how should shadowing be handled? Explicit or allowed? Allow for now.
        self.map.insert(
            name.clone(),
            Variable(cranelift::frontend::Variable::from_u32(self.index)),
        );
        self.index += 1;

        self.map.get(&name).unwrap()
    }
}

/// A [`Scope`] captures the variables that the code within the scope has access to.
///
/// Any syntax in TODO_LANG_NAME between a colon (":") and a semicolon (";") will have a scope.
///
/// [`Scope`] provides wrappers for [`Variable`] access that implement typical scoping rules.
/// When trying to access a variable, the [`VariableMap`] in the current scope will be checked first.
/// If the variable is not found, the outer scope will be checked. This will continue until there is no outer scope,
/// in which case a compiler error can be thrown.
#[derive(Debug)]
pub(super) struct Scope<'a> {
    outer_scope: Option<&'a Scope<'a>>,
    variables: VariableMap,
}

impl<'a> Scope<'a> {
    pub(super) fn new(outer_scope: Option<&'a Scope<'a>>) -> Self {
        Self {
            outer_scope,
            variables: VariableMap::new(),
        }
    }
}

impl Scope<'_> {
    pub(super) fn get_var(&self, name: &Identifier) -> Option<&Variable> {
        match self.variables.get(name) {
            Some(variable) => Some(variable),
            None => match &self.outer_scope {
                Some(outer_scope) => outer_scope.get_var(name),
                None => None,
            },
        }
    }

    pub(super) fn insert_var(&mut self, name: &Identifier) -> &Variable {
        self.variables.insert(name)
    }
}

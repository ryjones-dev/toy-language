use std::collections::HashMap;

use crate::parser::{function::FunctionSignature, identifier::Identifier, variable::Variable};

/// A [`Scope`] captures the variables and function signatures that the code within the scope has access to.
///
/// Any syntax in TODO_LANG_NAME between a colon (":") and a semicolon (";") will have a scope.
///
/// [`Scope`] provides wrappers for [`Variable`] and [`FunctionSignature`] access that implement typical scoping rules.
/// When trying to access a variable or function signature, the current scope will be checked first.
/// If the variable or function signature is not found, the outer scope will be checked.
/// This will continue until there is no outer scope, in which case a compiler error can be thrown.
#[derive(Debug)]
pub(super) struct Scope<'a> {
    outer_scope: Option<&'a Scope<'a>>,
    variables: HashMap<Identifier, Variable>,
    function_signatures: HashMap<Identifier, FunctionSignature>,
}

impl<'a> Scope<'a> {
    pub(super) fn new(outer_scope: Option<&'a Scope<'a>>) -> Self {
        Self {
            outer_scope,
            variables: HashMap::new(),
            function_signatures: HashMap::new(),
        }
    }
}

impl Scope<'_> {
    /// Returns a [`Variable`] with the given name, or [`None`] if the variable is not in scope.
    pub(super) fn get_var(&self, name: &Identifier) -> Option<&Variable> {
        match self.variables.get(name) {
            Some(variable) => Some(variable),
            None => match &self.outer_scope {
                Some(outer_scope) => outer_scope.get_var(name),
                None => None,
            },
        }
    }

    /// Adds the given [`Variable`] to the scope.
    ///
    /// Returns [`None`] if the variable was added successfully,
    /// or the previously defined [`Variable`] if it has already been defined.
    pub(super) fn insert_var(&mut self, variable: Variable) -> Option<&Variable> {
        // Attempting to insert a discarded variable is a no-op
        if variable.is_discarded() {
            return None;
        }

        if self.variables.contains_key(&variable.name) {
            return self.variables.get(&variable.name);
        }

        self.variables.insert(variable.name.clone(), variable);
        None
    }

    /// Returns a [`FunctionSignature`] with the given name, or [`None`] if the function is not in scope.
    pub(super) fn get_func_sig(&self, name: &Identifier) -> Option<&FunctionSignature> {
        match self.function_signatures.get(name) {
            Some(func_sig) => Some(func_sig),
            None => match &self.outer_scope {
                Some(outer_scope) => outer_scope.get_func_sig(name),
                None => None,
            },
        }
    }

    /// Adds the given [`FunctionSignature`] to the scope.
    ///
    ///
    /// Returns [`None`] if the function was added successfully,
    /// or the previously defined [`FunctionSignature`] if it has already been defined.
    pub(super) fn insert_func_sig(
        &mut self,
        func_sig: FunctionSignature,
    ) -> Option<&FunctionSignature> {
        if self.function_signatures.contains_key(&func_sig.name) {
            return self.function_signatures.get(&func_sig.name);
        }

        self.function_signatures
            .insert(func_sig.name.clone(), func_sig);
        None
    }
}

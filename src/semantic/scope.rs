use std::collections::HashMap;

use thiserror::Error;

use crate::ast_parser::types::{FunctionSignature, Identifier, Variable};

#[derive(Debug, Error)]
pub enum ScopeError {
    #[error("variable \"{0}\" is already defined in this scope")]
    VariableAlreadyDefinedError(Identifier),
    #[error("function \"{0}\" is already defined in this scope")]
    FunctionAlreadyDefinedError(Identifier),
}

/// A [`Scope`] captures the variables and function signatures that the code within the scope has access to.
///
/// Any syntax in TODO_LANG_NAME between a colon (":") and a semicolon (";") will have a scope.
///
/// [`Scope`] provides wrappers for [`Variable`] and [`FunctionSignature`] access that implement typical scoping rules.
/// When trying to access a variable or function signature, the current scope will be checked first.
/// If the variable or function signature is not found, the outer scope will be checked.
/// This will continue until there is no outer scope, in which case a compiler error can be thrown.
#[derive(Debug)]
pub(crate) struct Scope<'a> {
    outer_scope: Option<&'a Scope<'a>>,
    variables: HashMap<Identifier, Variable>,
    function_signatures: HashMap<Identifier, FunctionSignature>,
}

impl<'a> Scope<'a> {
    pub(crate) fn new(outer_scope: Option<&'a Scope<'a>>) -> Self {
        Self {
            outer_scope,
            variables: HashMap::new(),
            function_signatures: HashMap::new(),
        }
    }
}

impl Scope<'_> {
    pub(crate) fn get_var(&self, name: &Identifier) -> Option<&Variable> {
        match self.variables.get(name) {
            Some(variable) => Some(variable),
            None => match &self.outer_scope {
                Some(outer_scope) => outer_scope.get_var(name),
                None => None,
            },
        }
    }

    pub(crate) fn insert_var(&mut self, variable: Variable) -> Result<(), ScopeError> {
        if self.variables.contains_key(&variable.name) {
            return Err(ScopeError::VariableAlreadyDefinedError(variable.name));
        }

        self.variables.insert(variable.name.clone(), variable);
        Ok(())
    }

    pub(crate) fn get_func_sig(&self, name: &Identifier) -> Option<&FunctionSignature> {
        match self.function_signatures.get(name) {
            Some(func_sig) => Some(func_sig),
            None => match &self.outer_scope {
                Some(outer_scope) => outer_scope.get_func_sig(name),
                None => None,
            },
        }
    }

    pub(crate) fn insert_func_sig(
        &mut self,
        func_sig: FunctionSignature,
    ) -> Result<(), ScopeError> {
        if self.function_signatures.contains_key(&func_sig.name) {
            return Err(ScopeError::FunctionAlreadyDefinedError(func_sig.name));
        }

        self.function_signatures
            .insert(func_sig.name.clone(), func_sig);
        Ok(())
    }
}

use std::collections::HashMap;

use thiserror::Error;

use crate::ast_parser::types::{FunctionSignature, Identifier, Type};

/// Represents a reference to a defined variable.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) struct Variable {
    pub(super) var: cranelift::frontend::Variable,
    pub(super) ty: Type,
}

impl Variable {
    fn new(var: cranelift::frontend::Variable, ty: Type) -> Self {
        Self { var, ty }
    }
}

impl From<Variable> for cranelift::frontend::Variable {
    fn from(value: Variable) -> Self {
        value.var
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
            Variable::new(
                cranelift::frontend::Variable::from_u32(self.index),
                Type::Int, // TODO: support more than just an int type
            ),
        );
        self.index += 1;

        self.map.get(&name).unwrap()
    }
}

#[derive(Debug, Default)]
struct FunctionSignatureMap {
    map: HashMap<Identifier, FunctionSignature>,
}

impl FunctionSignatureMap {
    fn new() -> Self {
        Default::default()
    }

    fn get(&self, name: &Identifier) -> Option<&FunctionSignature> {
        self.map.get(name)
    }

    fn insert(&mut self, func_sig: &FunctionSignature) -> Result<(), ScopeError> {
        let function_name = func_sig.name.clone().unwrap(); // TODO: handle anonymous functions later
        if self.map.contains_key(&function_name) {
            return Err(ScopeError::FunctionAlreadyDefinedError(function_name));
        }

        self.map.insert(function_name.clone(), func_sig.clone());
        Ok(())
    }
}

#[derive(Debug, Error)]
pub(super) enum ScopeError {
    #[error("function \"{0}\" is already defined in this scope")]
    FunctionAlreadyDefinedError(Identifier),
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
pub(crate) struct Scope<'a> {
    outer_scope: Option<&'a Scope<'a>>,
    variables: VariableMap,
    function_signatures: FunctionSignatureMap,
}

impl<'a> Scope<'a> {
    pub(crate) fn new(outer_scope: Option<&'a Scope<'a>>) -> Self {
        Self {
            outer_scope,
            variables: VariableMap::new(),
            function_signatures: FunctionSignatureMap::new(),
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

    pub(crate) fn insert_var(&mut self, name: &Identifier) -> &Variable {
        self.variables.insert(name)
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
        func_sig: &FunctionSignature,
    ) -> Result<(), ScopeError> {
        self.function_signatures.insert(func_sig)
    }
}

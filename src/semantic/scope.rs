use std::{
    cell::{Ref, RefCell},
    collections::HashMap,
};

use crate::parser::{function::FunctionSignature, identifier::Identifier, variable::Variable};

/// A wrapping struct to keep track of how many times a [`Variable`] is read.
/// This is later used to determine unused variables.
#[derive(Debug)]
struct VariableMetadata {
    variable: Variable,
    read_count: i32,
}

/// A wrapping struct to keep track of how many times a [`FunctionSignature`] is read.
/// This is later used to determine unused functions.
#[derive(Debug)]
struct FunctionMetadata {
    function_signature: FunctionSignature,
    read_count: i32,
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
pub(super) struct Scope<'a> {
    outer_scope: Option<&'a Scope<'a>>,

    // RefCell is needed so we can update metadata during read access
    variable_metadata: HashMap<Identifier, RefCell<VariableMetadata>>,
    function_metadata: HashMap<Identifier, RefCell<FunctionMetadata>>,
}

impl<'a> Scope<'a> {
    pub(super) fn new(outer_scope: Option<&'a Scope<'a>>) -> Self {
        Self {
            outer_scope,
            variable_metadata: HashMap::new(),
            function_metadata: HashMap::new(),
        }
    }
}

impl Scope<'_> {
    /// Returns a [`Variable`] with the given name, or [`None`] if the variable is not in scope.
    pub(super) fn get_var(&self, name: &Identifier) -> Option<Ref<Variable>> {
        match self.variable_metadata.get(name) {
            Some(var_meta) => {
                var_meta.borrow_mut().read_count += 1;
                Some(Ref::map(var_meta.borrow(), |var_meta| &var_meta.variable))
            }
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
    pub(super) fn insert_var(&mut self, variable: Variable) -> Option<Ref<Variable>> {
        if self.variable_metadata.contains_key(variable.name()) {
            return self
                .variable_metadata
                .get(variable.name())
                .map(|var_meta| Ref::map(var_meta.borrow(), |var_meta| &var_meta.variable));
        }

        self.variable_metadata.insert(
            variable.name().clone(),
            RefCell::new(VariableMetadata {
                variable,
                read_count: 0,
            }),
        );
        None
    }

    /// Returns a [`FunctionSignature`] with the given name, or [`None`] if the function is not in scope.
    pub(super) fn get_func_sig(&self, name: &Identifier) -> Option<Ref<FunctionSignature>> {
        match self.function_metadata.get(name) {
            Some(func_meta) => {
                func_meta.borrow_mut().read_count += 1;
                Some(Ref::map(func_meta.borrow(), |func_meta| {
                    &func_meta.function_signature
                }))
            }
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
    ) -> Option<Ref<FunctionSignature>> {
        if self.function_metadata.contains_key(&func_sig.name) {
            return self.function_metadata.get(&func_sig.name).map(|func_meta| {
                Ref::map(func_meta.borrow(), |func_meta| {
                    &func_meta.function_signature
                })
            });
        }

        self.function_metadata.insert(
            func_sig.name.clone(),
            RefCell::new(FunctionMetadata {
                function_signature: func_sig,
                read_count: 0,
            }),
        );
        None
    }

    /// Consumes the scope and returns a list of [`Variable`]s and [`FunctionSignature`]s
    /// that have not been read from in this scope.
    ///
    /// This intentionally does not check outer scopes, as those still have the potential to be used.
    ///
    /// This method does not use the [`Variables`] type because these variables are not related to each other,
    /// and don't have consistent source ranges.
    pub(super) fn get_unused(self) -> (Vec<Variable>, Vec<FunctionSignature>) {
        let mut unused_variables = Vec::new();
        let mut unused_functions = Vec::new();

        for (_, mut var_meta) in self.variable_metadata {
            // Ignore discarded variables
            if !var_meta.get_mut().variable.is_discarded() && var_meta.get_mut().read_count == 0 {
                unused_variables.push(var_meta.get_mut().variable.clone());
            }
        }

        for (_, mut func_meta) in self.function_metadata {
            // Ignore discarded functions
            if !func_meta.get_mut().function_signature.is_discarded()
                && func_meta.get_mut().read_count == 0
            {
                unused_functions.push(func_meta.get_mut().function_signature.clone());
            }
        }

        (unused_variables, unused_functions)
    }
}

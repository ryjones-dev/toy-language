use std::{
    cell::{Ref, RefCell},
    collections::HashMap,
};

use crate::parser::{function::FunctionSignature, r#struct::Struct, variable::Variable};

use super::DataType;

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

/// A wrapping struct to keep track of how many times a [`Struct`] is read.
/// This is later used to determine unused structs.
#[derive(Debug)]
struct StructMetadata {
    _struct: Struct,
    read_count: RefCell<i32>,
}

/// A [`ScopeTracker`] captures the definitions and variables that the code within a scope has access to.
///
/// Every [`Scope`] will have a corresponding [`ScopeTracker`].
///
/// [`ScopeTracker`] provides wrappers for [`Variable`], [`FunctionSignature`], and [`Struct`] access that implement typical scoping rules.
/// When trying to access one of these code primitives, the current scope will be checked first.
/// If the code primitive is not found, the outer scope will be checked.
/// This will continue until there is no outer scope, in which case a compiler error can be thrown.
#[derive(Debug)]
pub(super) struct ScopeTracker<'a> {
    outer_scope: Option<&'a ScopeTracker<'a>>,

    // RefCell is needed so we can update metadata during read access
    variable_metadata: HashMap<String, RefCell<VariableMetadata>>,
    function_metadata: HashMap<String, RefCell<FunctionMetadata>>,
    struct_metadata: HashMap<String, StructMetadata>,
}

impl<'a> ScopeTracker<'a> {
    pub(super) fn new(outer_scope: Option<&'a ScopeTracker<'a>>) -> Self {
        Self {
            outer_scope,
            variable_metadata: HashMap::new(),
            function_metadata: HashMap::new(),
            struct_metadata: HashMap::new(),
        }
    }
}

impl ScopeTracker<'_> {
    /// Returns a [`Variable`] with the given name, or [`None`] if the variable is not in scope.
    pub(super) fn get_var<'s>(
        &self,
        name: impl Into<&'s str> + Eq + std::hash::Hash,
    ) -> Option<Ref<Variable>> {
        let name = name.into();
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
        let var_name: &str = variable.name().into();
        if self.variable_metadata.contains_key(var_name) {
            return self
                .variable_metadata
                .get(var_name)
                .map(|var_meta| Ref::map(var_meta.borrow(), |var_meta| &var_meta.variable));
        }

        self.variable_metadata.insert(
            var_name.to_string(),
            RefCell::new(VariableMetadata {
                variable,
                read_count: 0,
            }),
        );
        None
    }

    /// Returns a [`FunctionSignature`] with the given name, or [`None`] if the function is not in scope.
    pub(super) fn get_func_sig<'s>(
        &self,
        name: impl Into<&'s str> + Eq + std::hash::Hash,
    ) -> Option<Ref<FunctionSignature>> {
        let name = name.into();
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

    pub(super) fn has_main_func(&self) -> bool {
        if let None = self.get_func_sig("main") {
            false
        } else {
            true
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
        let func_name: &str = (&func_sig.name).into();
        if self.function_metadata.contains_key(func_name) {
            return self.function_metadata.get(func_name).map(|func_meta| {
                Ref::map(func_meta.borrow(), |func_meta| {
                    &func_meta.function_signature
                })
            });
        }

        self.function_metadata.insert(
            func_name.to_string(),
            RefCell::new(FunctionMetadata {
                function_signature: func_sig,
                read_count: 0,
            }),
        );
        None
    }

    /// Returns a [`Struct`] with the given name, or [`None`] if the struct is not in scope.
    pub(super) fn get_struct<'s>(
        &self,
        name: impl Into<&'s str> + Eq + std::hash::Hash,
    ) -> Option<&Struct> {
        let name = name.into();
        match self.struct_metadata.get(name) {
            Some(struct_meta) => {
                *struct_meta.read_count.borrow_mut() += 1;
                Some(&struct_meta._struct)
            }
            None => match &self.outer_scope {
                Some(outer_scope) => outer_scope.get_struct(name),
                None => None,
            },
        }
    }

    // Internal use only. Used to get a mutable reference to a struct so that its member
    // data types can be updated once the struct types are known.
    fn get_struct_mut<'s>(
        &mut self,
        name: impl Into<&'s str> + Eq + std::hash::Hash,
    ) -> Option<&mut Struct> {
        let name = name.into();
        match self.struct_metadata.get_mut(name) {
            Some(struct_meta) => {
                *struct_meta.read_count.get_mut() += 1;
                Some(&mut struct_meta._struct)
            }
            None => panic!("can only get mutable references to structs in the current scope"),
        }
    }

    /// Adds the given [`Struct`] to the scope.
    ///
    /// Returns [`None`] if the variable was added successfully,
    /// or the previously defined [`Struct`] if it has already been defined.
    pub(super) fn insert_struct(&mut self, _struct: Struct) -> Option<&Struct> {
        let struct_name: &str = _struct.name().into();
        if self.struct_metadata.contains_key(struct_name) {
            return self
                .struct_metadata
                .get(struct_name)
                .map(|struct_meta| &struct_meta._struct);
        }

        self.struct_metadata.insert(
            struct_name.to_string(),
            StructMetadata {
                _struct,
                read_count: RefCell::new(0),
            },
        );
        None
    }

    /// Finds the given struct member in the given struct that has a [`Struct`] data type,
    /// and updates that data type with the [`DataType`]s of that [`Struct`].
    pub(super) fn update_struct_data_types<'s>(
        &mut self,
        name: impl Into<&'s str> + Eq + std::hash::Hash,
        member_name: impl Into<&'s str> + Eq,
        data_types: Vec<DataType>,
    ) {
        let member_name = member_name.into();
        if let Some(_struct) = self.get_struct_mut(name) {
            for member in _struct.members_mut() {
                if member.name().to_string() == member_name {
                    if let &mut DataType::Struct {
                        ref mut struct_data_types,
                        ..
                    } = member.get_type_mut().into()
                    {
                        *struct_data_types = Some(data_types.clone());
                        break;
                    }
                }
            }
        }
    }

    /// Consumes the [`ScopeTracker`] and returns a list of [`Variable`]s, [`FunctionSignature`]s, and [`Struct`]s
    /// that have not been read from in this scope.
    ///
    /// This intentionally does not check outer scopes, as those still have the potential to be used.
    ///
    /// This method does not use the [`Variables`] type because these variables are not related to each other,
    /// and don't have consistent source ranges.
    pub(super) fn get_unused(self) -> (Vec<Variable>, Vec<FunctionSignature>, Vec<Struct>) {
        let mut unused_variables = Vec::new();
        let mut unused_functions = Vec::new();
        let mut unused_structs = Vec::new();

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

        for (_, struct_meta) in self.struct_metadata {
            // Ignore discarded structs
            if !struct_meta._struct.is_discarded() && struct_meta.read_count == 0.into() {
                unused_structs.push(struct_meta._struct);
            }
        }

        (unused_variables, unused_functions, unused_structs)
    }
}

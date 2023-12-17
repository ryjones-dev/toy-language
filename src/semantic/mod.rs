use thiserror::Error;

use crate::ast_parser::types::{AbstractSyntaxTree, Identifier, Statement, Type, Types};

use self::{
    expression::{analyze_expression, analyze_function_call, ExpressionError},
    scope::{Scope, ScopeError},
};

mod expression;
pub(super) mod scope;

#[derive(Debug, Error)]
pub enum SemanticError {
    #[error("main function is not defined")]
    MissingMainError,
    #[error("anonymous functions cannot be defined in the global scope")]
    AnonymousFunctionInGlobalScopeError,
    #[error("function \"{0}\" returns values that are not stored in a variable. If this is intentional, use the discard identifier (\"_\")")]
    NonZeroReturnError(Identifier),
    #[error("{0}")]
    ExpressionError(#[from] ExpressionError),
    #[error("{0}")]
    ScopeError(#[from] ScopeError),
}

pub(crate) fn semantic_analysis(ast: &AbstractSyntaxTree) -> Result<(), Vec<SemanticError>> {
    let mut errors = Vec::new();

    let mut global_scope = Scope::new(None);

    let mut has_main_function = false;

    // Insert each function signature in the global scope
    for function in ast.iter() {
        // It's not possible for anonymous functions to be defined multiple times
        if let Some(function_name) = &function.signature.name {
            if let Err(err) = global_scope.insert_func_sig(&function.signature) {
                errors.push(SemanticError::ScopeError(err));
            } else {
                if function_name.to_string() == "main" {
                    has_main_function = true;
                }
            }
        } else {
            // Anonymous functions can't be defined in the global scope
            errors.push(SemanticError::AnonymousFunctionInGlobalScopeError);
        }
    }

    if !has_main_function {
        errors.push(SemanticError::MissingMainError);
    }

    // Check for errors in each function
    for function in ast.iter() {
        let mut function_scope = Scope::new(Some(&global_scope));

        // Insert parameters into function scope
        for param in &function.signature.params {
            function_scope.insert_var(param);
        }

        for statement in &function.body {
            match statement {
                Statement::Assignment(variable_names, expression) => {
                    match analyze_expression(&expression, &function_scope) {
                        Ok(types) => {
                            // TODO: check variable types against returned types instead of just the length
                            if types.len() != variable_names.len() {
                                errors.push(SemanticError::ExpressionError(
                                    ExpressionError::MismatchedAssignmentError {
                                        expected: types,
                                        actual: variable_names.iter().map(|_| Type::Int).collect(), // TODO: THIS IS A HACK. Need to actually know the types of the variables here.
                                    },
                                ));
                            }
                        }
                        Err(err) => errors.push(SemanticError::ExpressionError(err)),
                    }

                    // Always add the variables to the scope so that additional errors are not unnecessarily added downstream
                    for variable_name in variable_names {
                        if let Some(variable_name) = variable_name {
                            function_scope.insert_var(variable_name);
                        }
                    }
                }
                Statement::FunctionCall(function_call) => {
                    match analyze_function_call(function_call, &function_scope) {
                        Ok(types) => {
                            // Ensure that function calls do not return a value, otherwise an assignment statement needs to be used
                            if types.len() > 0 {
                                errors.push(SemanticError::NonZeroReturnError(
                                    function_call.name.clone(),
                                ))
                            }
                        }
                        Err(err) => errors.push(SemanticError::ExpressionError(err)),
                    }
                }
                Statement::Return(expressions) => {
                    let mut return_types = Types::new();
                    let mut has_expression_error = false;

                    for expression in expressions {
                        match analyze_expression(&expression, &function_scope) {
                            Ok(mut types) => {
                                return_types.append(&mut types);
                            }
                            Err(err) => {
                                errors.push(SemanticError::ExpressionError(err));
                                has_expression_error = true;
                            }
                        }
                    }

                    // Ensure that the function returns the correct types.
                    // Ignore this check if there was a previous expression error as to not
                    // add irrelevant errors to the error list.
                    if !has_expression_error && function.signature.returns != return_types {
                        errors.push(SemanticError::ExpressionError(
                            ExpressionError::MismatchedReturnError {
                                function_name: function.signature.name.clone(),
                                expected: function.signature.returns.clone(),
                                actual: return_types,
                            },
                        ))
                    }
                }
            }
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

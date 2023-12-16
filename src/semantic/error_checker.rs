use thiserror::Error;

use crate::ast_parser::types::{AbstractSyntaxTree, Function, Identifier, Statement, Type};

use super::{
    expression::{check_expression, check_function_call, ExpressionError},
    scope::{Scope, ScopeError},
};

#[derive(Debug, Error)]
pub(crate) enum SemanticError {
    #[error("function \"{name}\" is already defined in this scope")]
    FunctionAlreadyDefinedError { name: Identifier },
    #[error("anonymous functions cannot be defined in the global scope")]
    AnonymousFunctionInGlobalScopeError,
    #[error("{0}")]
    ExpressionError(#[from] ExpressionError),
    #[error("{0}")]
    ScopeError(#[from] ScopeError),
}

fn check_function_already_defined(
    scope: &mut Scope,
    function: &Function,
) -> Result<(), SemanticError> {
    scope
        .insert_func_sig(&function.signature)
        .map_err(|err| SemanticError::ScopeError(err))?;
    Ok(())
}

pub(crate) fn check(ast: &AbstractSyntaxTree) -> Result<(), Vec<SemanticError>> {
    let mut errors = Vec::new();

    let mut global_scope = Scope::new(None);

    // Check for errors in each function
    for function in ast.iter() {
        // First, check if the function has already been defined
        if let Some(_) = &function.signature.name {
            if let Err(err) = check_function_already_defined(&mut global_scope, function) {
                errors.push(err);
            }
        } else {
            // Anonymous functions can't be defined in the global scope
            errors.push(SemanticError::AnonymousFunctionInGlobalScopeError);
        }

        let mut function_scope = Scope::new(Some(&global_scope));

        // Insert parameters into function scope
        for param in &function.signature.params {
            function_scope.insert_var(param);
        }

        for statement in &function.body {
            match statement {
                Statement::Assignment(variable_names, expression) => {
                    match check_expression(&expression, &function_scope) {
                        Ok(types) => {
                            // TODO: check variable types against returned types instead of just the length
                            if variable_names.len() != types.len() {
                                errors.push(SemanticError::ExpressionError(
                                    ExpressionError::MismatchedExpressionResultsError {
                                        expected: variable_names
                                            .iter()
                                            .map(|_| Type::Int)
                                            .collect(), // TODO: THIS IS A HACK. Need to actually know the types of the variables here.
                                        actual: types,
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
                    match check_function_call(function_call, &function_scope) {
                        Ok(types) => {
                            // Ensure that function calls do not return a value, otherwise an assignment statement needs to be used
                            if types.len() > 0 {
                                errors.push(SemanticError::ExpressionError(
                                    ExpressionError::MismatchedExpressionResultsError {
                                        expected: Vec::new(),
                                        actual: types,
                                    },
                                ))
                            }
                        }
                        Err(err) => errors.push(SemanticError::ExpressionError(err)),
                    }
                }
                Statement::Return(expressions) => {
                    let mut return_types = Vec::new();
                    let mut has_expression_error = false;

                    for expression in expressions {
                        match check_expression(&expression, &function_scope) {
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
                            ExpressionError::MismatchedExpressionResultsError {
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

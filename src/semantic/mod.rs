use thiserror::Error;

use crate::parser::types::{AbstractSyntaxTree, Identifier, Statement, Types};

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
    #[error("function \"{0}\" returns values that are not stored in a variable. if this is intentional, use the discard identifier (\"_\")")]
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
        if let Err(err) = global_scope.insert_func_sig(function.signature.clone()) {
            errors.push(SemanticError::ScopeError(err));
        } else {
            if function.signature.name.to_string() == "main" {
                has_main_function = true;
            }
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
            if let Err(err) = function_scope.insert_var(param.clone()) {
                errors.push(SemanticError::ScopeError(err));
            }
        }

        for statement in &function.body {
            match statement {
                Statement::Assignment(variables, expression) => {
                    match analyze_expression(&expression, &function_scope) {
                        Ok(types) => {
                            if types.len() != variables.len() {
                                errors.push(SemanticError::ExpressionError(
                                    ExpressionError::WrongNumberOfVariablesError {
                                        expected: types.len(),
                                        actual: variables.len(),
                                    },
                                ));
                            }

                            for (i, variable) in variables.iter().enumerate() {
                                if let Some(variable) = variable {
                                    if variable.ty != types[i] {
                                        errors.push(SemanticError::ExpressionError(
                                            ExpressionError::MismatchedTypeAssignmentError {
                                                expected: types[i],
                                                actual: variable.ty,
                                            },
                                        ));
                                    }
                                }
                            }
                        }
                        Err(err) => errors.push(SemanticError::ExpressionError(err)),
                    }

                    // Always add the variables to the scope so that additional errors are not unnecessarily added downstream
                    for variable in variables {
                        if let Some(variable_name) = variable {
                            if let Err(err) = function_scope.insert_var(variable_name.clone()) {
                                errors.push(SemanticError::ScopeError(err));
                            }
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
                    // Ignore these checks if there was a previous expression error as to not
                    // add irrelevant errors to the error list.
                    if !has_expression_error {
                        if function.signature.returns != return_types {
                            errors.push(SemanticError::ExpressionError(
                                ExpressionError::WrongNumberOfReturnValuesError {
                                    function_name: function.signature.name.clone(),
                                    expected: function.signature.returns.len(),
                                    actual: return_types.len(),
                                },
                            ))
                        }

                        for (i, return_type) in function.signature.returns.iter().enumerate() {
                            if *return_type != return_types[i] {
                                errors.push(SemanticError::ExpressionError(
                                    ExpressionError::MismatchedReturnValueTypeError {
                                        function_name: function.signature.name.clone(),
                                        expected: *return_type,
                                        actual: return_types[i],
                                    },
                                ))
                            }
                        }
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

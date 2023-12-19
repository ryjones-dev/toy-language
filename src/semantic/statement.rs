use thiserror::Error;

use crate::parser::types::{FunctionSignature, Identifier, Statement, Type, Types};

use super::{
    expression::{analyze_expression, analyze_function_call, ExpressionError},
    scope::{Scope, ScopeError},
};

#[derive(Debug, Error)]
pub enum StatementError {
    #[error("wrong number of variables in expression assignment. expected: {expected}, actual: {actual}")]
    WrongNumberOfVariablesError { expected: usize, actual: usize },
    #[error(
        "mismatched variable type in expression assignment. expected: {expected}, actual: {actual}"
    )]
    MismatchedTypeAssignmentError { expected: Type, actual: Type },
    #[error("function \"{0}\" returns values that are not stored in a variable. if this is intentional, use the discard identifier (\"_\")")]
    NonZeroReturnError(Identifier),
    #[error("wrong number of return values for function \"{function_name}\". expected: {expected}, actual: {actual}")]
    WrongNumberOfReturnValuesError {
        function_name: Identifier,
        expected: usize,
        actual: usize,
    },
    #[error("mismatched return value type for function \"{function_name}\". expected: {expected}, actual: {actual}")]
    MismatchedReturnValueTypeError {
        function_name: Identifier,
        expected: Type,
        actual: Type,
    },
    #[error("{0}")]
    ExpressionError(#[from] ExpressionError),
    #[error("{0}")]
    ScopeError(#[from] ScopeError),
}

pub(super) fn analyze_statement(
    func_sig: &FunctionSignature,
    scope: &mut Scope,
    statement: &Statement,
) -> Result<(), Vec<StatementError>> {
    let mut errors = Vec::new();
    match statement {
        Statement::Assignment(variables, expression) => {
            match analyze_expression(&expression, &scope) {
                Ok(types) => {
                    if types.len() != variables.len() {
                        errors.push(StatementError::WrongNumberOfVariablesError {
                            expected: types.len(),
                            actual: variables.len(),
                        });
                    }

                    for (i, variable) in variables.iter().enumerate() {
                        if let Some(variable) = variable {
                            if variable.ty != types[i] {
                                errors.push(StatementError::MismatchedTypeAssignmentError {
                                    expected: types[i],
                                    actual: variable.ty,
                                });
                            }
                        }
                    }
                }
                Err(err) => errors.push(StatementError::ExpressionError(err)),
            }

            // Always add the variables to the scope so that additional errors are not unnecessarily added downstream
            for variable in variables {
                if let Some(variable_name) = variable {
                    if let Err(err) = scope.insert_var(variable_name.clone()) {
                        errors.push(StatementError::ScopeError(err));
                    }
                }
            }
        }
        Statement::FunctionCall(function_call) => {
            match analyze_function_call(function_call, &scope) {
                Ok(types) => {
                    // Ensure that function calls do not return a value, otherwise an assignment statement needs to be used
                    if types.len() > 0 {
                        errors.push(StatementError::NonZeroReturnError(
                            function_call.name.clone(),
                        ))
                    }
                }
                Err(err) => errors.push(StatementError::ExpressionError(err)),
            }
        }
        Statement::Return(expressions) => {
            let mut return_types = Types::new();
            let mut has_expression_error = false;

            for expression in expressions {
                match analyze_expression(&expression, &scope) {
                    Ok(mut types) => {
                        return_types.append(&mut types);
                    }
                    Err(err) => {
                        errors.push(StatementError::ExpressionError(err));
                        has_expression_error = true;
                    }
                }
            }

            // Ensure that the function returns the correct types.
            // Ignore these checks if there was a previous expression error as to not
            // add irrelevant errors to the error list.
            if !has_expression_error {
                if func_sig.returns != return_types {
                    errors.push(StatementError::WrongNumberOfReturnValuesError {
                        function_name: func_sig.name.clone(),
                        expected: func_sig.returns.len(),
                        actual: return_types.len(),
                    })
                }

                for (i, return_type) in func_sig.returns.iter().enumerate() {
                    if *return_type != return_types[i] {
                        errors.push(StatementError::MismatchedReturnValueTypeError {
                            function_name: func_sig.name.clone(),
                            expected: *return_type,
                            actual: return_types[i],
                        })
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

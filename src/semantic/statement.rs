use thiserror::Error;

use crate::{
    diagnostic::{Diagnostic, DiagnosticContext, DiagnosticLevel, DiagnosticMessage},
    parser::{
        function::{FunctionCall, FunctionSignature},
        statement::Statement,
        types::{Type, Types},
        variable::Variable,
    },
};

use super::{
    diagnostic::{diag_expected_types, diag_func_name_label, diag_func_sig_return_label},
    expression::{analyze_expression, analyze_function_call, ExpressionError},
    scope::{Scope, ScopeError},
};

#[derive(Debug, Error)]
pub(super) enum StatementError {
    #[error("wrong number of variables in expression assignment")]
    WrongNumberOfVariablesError {
        expected: Types,
        actual: Vec<Variable>,
    },
    #[error(
        "mismatched variable type in expression assignment for \"{actual}\". expected: {expected}, actual: {}",
        if .actual.ty.is_some() { .actual.ty.unwrap().to_string() } else { "unknown".to_string() }
    )]
    MismatchedTypeAssignmentError { expected: Type, actual: Variable },
    #[error("function return values are not stored in a variable")]
    NonZeroReturnError {
        func_sig: FunctionSignature,
        function_call: FunctionCall,
    },
    #[error("return value mismatch")]
    ReturnValueMismatchError {
        func_sig: FunctionSignature,
        return_types: Types,
    },
    #[error(transparent)]
    ExpressionError(#[from] ExpressionError),
    #[error(transparent)]
    ScopeError(#[from] ScopeError),
}

impl From<StatementError> for Diagnostic {
    fn from(err: StatementError) -> Self {
        match err {
            StatementError::WrongNumberOfVariablesError { expected, actual } => todo!(),
            StatementError::MismatchedTypeAssignmentError { expected, actual } => todo!(),
            StatementError::NonZeroReturnError {
                ref func_sig,
                ref function_call,
            } => Self::new(&err, DiagnosticLevel::Error)
                .with_context(
                    DiagnosticContext::new(DiagnosticMessage::new(
                        "function called here",
                        function_call.source,
                    ))
                    .with_labels(vec![
                        diag_func_name_label(func_sig),
                        diag_func_sig_return_label(func_sig),
                    ]),
                )
                .with_suggestion(
                    "if the results are not needed, \
                    assign the results to a discarded variable \"_\"",
                ),
            StatementError::ReturnValueMismatchError {
                ref func_sig,
                ref return_types,
            } => Self::new(&err, DiagnosticLevel::Error).with_context(
                DiagnosticContext::new(diag_expected_types(&func_sig.returns, return_types))
                    .with_labels({
                        let mut labels = vec![diag_func_name_label(&func_sig)];
                        if func_sig.returns.len() > 0 {
                            labels.push(diag_func_sig_return_label(&func_sig));
                        }
                        labels
                    }),
            ),

            StatementError::ExpressionError(err) => err.into(),
            StatementError::ScopeError(err) => err.into(),
        }
    }
}

pub(super) fn analyze_statement(
    func_sig: &FunctionSignature,
    scope: &mut Scope,
    statement: &mut Statement,
) -> Result<(), Vec<StatementError>> {
    let mut errors = Vec::new();
    match statement {
        Statement::Assignment {
            variables,
            expression,
            source,
        } => {
            let (types, errs) = analyze_expression(expression, &scope);
            errors.append(
                &mut errs
                    .into_iter()
                    .map(|err| StatementError::ExpressionError(err))
                    .collect(),
            );

            if types.len() != variables.len() {
                errors.push(StatementError::WrongNumberOfVariablesError {
                    expected: types,
                    actual: variables.clone(),
                });
            } else {
                for (i, variable) in variables.iter_mut().enumerate() {
                    // If the variable type is None, this is a new variable definition.
                    // Set the variable type to the corresponding expression result type.
                    if variable.ty == None {
                        variable.ty = Some(types[i].into());
                    }

                    if variable.ty.unwrap() != types[i] {
                        errors.push(StatementError::MismatchedTypeAssignmentError {
                            expected: types[i],
                            actual: variable.clone(),
                        });
                    }
                }
            }

            // Always add the variables to the scope so that additional errors are not unnecessarily added downstream
            for variable in variables {
                if let Err(err) = scope.insert_var(variable.clone()) {
                    errors.push(StatementError::ScopeError(err));
                }
            }
        }
        Statement::FunctionCall(function_call) => {
            let (func_sig, errs) = analyze_function_call(function_call, &scope);

            errors.append(
                &mut errs
                    .into_iter()
                    .map(|err| StatementError::ExpressionError(err))
                    .collect(),
            );

            match func_sig {
                Some(func_sig) => {
                    // Ensure that function calls do not return a value, otherwise an assignment statement needs to be used
                    if func_sig.returns.len() > 0 {
                        errors.push(StatementError::NonZeroReturnError {
                            func_sig: func_sig.clone(),
                            function_call: function_call.clone(),
                        });
                    }
                }
                None => {}
            };
        }
        Statement::Return { expressions, .. } => {
            let mut return_types = Types::new();

            for expression in expressions {
                let (mut types, errs) = analyze_expression(expression, &scope);
                errors.append(
                    &mut errs
                        .into_iter()
                        .map(|err| StatementError::ExpressionError(err))
                        .collect(),
                );

                return_types.append(&mut types);
            }

            // Ensure that the function returns the correct types
            if func_sig.returns.len() != return_types.len() {
                errors.push(StatementError::ReturnValueMismatchError {
                    func_sig: func_sig.clone(),
                    return_types,
                })
            } else {
                for (i, return_type) in func_sig.returns.iter().enumerate() {
                    if *return_type != return_types[i] {
                        errors.push(StatementError::ReturnValueMismatchError {
                            func_sig: func_sig.clone(),
                            return_types: return_types.clone(),
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

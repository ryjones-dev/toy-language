use thiserror::Error;

use crate::{
    diagnostic::{Diagnostic, DiagnosticContext, DiagnosticLevel, DiagnosticMessage},
    parser::{
        function::FunctionSignature,
        identifier::Identifier,
        statement::Statement,
        types::{Type, Types},
        variable::Variable,
    },
};

use super::{
    expression::{analyze_expression, analyze_function_call, ExpressionError},
    scope::{Scope, ScopeError},
    EXPECT_VAR_TYPE,
};

#[derive(Debug, Error)]
pub enum StatementError {
    #[error("wrong number of variables in expression assignment")]
    WrongNumberOfVariablesError {
        expected: Types,
        actual: Vec<Option<Variable>>,
    },
    #[error(
        "mismatched variable type in expression assignment for \"{actual}\". expected: {expected}, actual: {}",
        if .actual.ty.is_some() { .actual.ty.unwrap().to_string() } else { "unknown".to_string() }
    )]
    MismatchedTypeAssignmentError { expected: Type, actual: Variable },
    #[error("function \"{0}\" returns values that are not stored in a variable. if this is intentional, use the discard identifier (\"_\")")]
    NonZeroReturnError(Identifier),
    #[error("wrong number of return values")]
    WrongNumberOfReturnValuesError {
        func_sig: FunctionSignature,
        return_types: Types,
    },
    #[error("mismatched return value type")]
    MismatchedReturnValueTypeError {
        function_name: Identifier,
        expected: Type,
        actual: Type,
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
            StatementError::NonZeroReturnError(_) => todo!(),
            StatementError::WrongNumberOfReturnValuesError {
                ref func_sig,
                ref return_types,
            } => {
                let mut labels = vec![DiagnosticMessage::new(
                    format!("for function `{}`", func_sig.name),
                    func_sig.name.source,
                )];

                if func_sig.returns.len() > 0 {
                    labels.push(DiagnosticMessage::new(
                        format!("return types defined here"),
                        func_sig.returns.source().unwrap(),
                    ));
                }

                Self::new(err.to_string(), DiagnosticLevel::Error).with_context(
                    if return_types.len() > 0 {
                        DiagnosticContext::new(DiagnosticMessage::new(
                            format!("expected `{}`, found `{}`", func_sig.returns, return_types),
                            return_types.source().unwrap(),
                        ))
                    } else {
                        DiagnosticContext::new(DiagnosticMessage::new(
                            format!(
                                "expected `{}`, found `{}`",
                                func_sig.returns.len(),
                                return_types.len()
                            ),
                            // Function signature return types cannot be 0 in this branch
                            func_sig.returns.source().unwrap(),
                        ))
                    }
                    .with_labels(labels),
                )
            }
            StatementError::MismatchedReturnValueTypeError {
                ref function_name,
                expected,
                actual,
            } => Self::new(err.to_string(), DiagnosticLevel::Error).with_context(
                DiagnosticContext::new(DiagnosticMessage::new(
                    format!("expected `{expected}`, found `{actual}`"),
                    actual.source,
                ))
                .with_labels(vec![
                    DiagnosticMessage::new(
                        format!("for function `{function_name}`"),
                        function_name.source,
                    ),
                    DiagnosticMessage::new(format!("return types defined here"), expected.source),
                ]),
            ),
            StatementError::ExpressionError(_) => todo!(),
            StatementError::ScopeError(_) => todo!(),
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
        Statement::Assignment(variables, expression) => {
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
                    if let Some(variable) = variable {
                        // If the variable type is None, this is a new variable definition.
                        // Set the variable type to the corresponding expression result type.
                        if variable.ty == None {
                            variable.ty = Some(types[i].ty);
                        }

                        let var_type = variable.ty.expect(EXPECT_VAR_TYPE);
                        if var_type != types[i].ty {
                            errors.push(StatementError::MismatchedTypeAssignmentError {
                                expected: types[i],
                                actual: variable.clone(),
                            });
                        }
                    }
                }
            }

            // Always add the variables to the scope so that additional errors are not unnecessarily added downstream
            for variable in variables {
                if let Some(variable) = variable {
                    if let Err(err) = scope.insert_var(variable.clone()) {
                        errors.push(StatementError::ScopeError(err));
                    }
                }
            }
        }
        Statement::FunctionCall(function_call) => {
            let (types, errs) = analyze_function_call(function_call, &scope);
            errors.append(
                &mut errs
                    .into_iter()
                    .map(|err| StatementError::ExpressionError(err))
                    .collect(),
            );

            // Ensure that function calls do not return a value, otherwise an assignment statement needs to be used
            if types.len() > 0 {
                errors.push(StatementError::NonZeroReturnError(
                    function_call.name.clone(),
                ))
            }
        }
        Statement::Return(expressions) => {
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
                errors.push(StatementError::WrongNumberOfReturnValuesError {
                    func_sig: func_sig.clone(),
                    return_types: return_types,
                })
            } else {
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

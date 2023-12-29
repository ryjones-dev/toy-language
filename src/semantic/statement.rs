use thiserror::Error;

use crate::{
    diagnostic::{Diagnostic, DiagnosticContext, DiagnosticLevel, DiagnosticMessage},
    parser::{
        expression::Expression,
        function::{FunctionCall, FunctionSignature},
        source_range::SourceRange,
        statement::Statement,
        types::{Type, Types},
        variable::{Variable, Variables},
    },
};

use super::{
    diagnostic::{
        diag_expected, diag_expected_types, diag_func_name_label, diag_newly_defined,
        diag_originally_defined, diag_return_types_label,
    },
    expression::{analyze_expression, analyze_function_call, ExpressionError},
    scope::Scope,
    EXPECT_TYPES, EXPECT_VAR_TYPE,
};

#[derive(Debug, Error)]
pub(super) enum StatementError {
    #[error("wrong number of variables in assignment")]
    WrongNumberOfVariablesError {
        expression: Expression,
        expected: Types,
        actual: Variables,
    },
    #[error("assignment type mismatch")]
    AssignmentTypeMismatchError {
        expected_type: Type,
        prev_var: Variable,
        var: Variable,
        assignment_source: SourceRange,
        expression: Expression,
    },
    #[error("function result{} not stored", if .func_sig.returns.len() == 1 { " is" } else { "s are" })]
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
}

impl From<StatementError> for Diagnostic {
    fn from(err: StatementError) -> Self {
        match err {
            StatementError::WrongNumberOfVariablesError {
                ref expression,
                ref expected,
                ref actual,
            } => Self::new(&err, DiagnosticLevel::Error).with_context(
                DiagnosticContext::new(diag_expected(
                    &expected.len(),
                    &actual.len(),
                    actual.source().unwrap(), // There must be at least 1 variable for this error to occur
                ))
                .with_labels({
                    let mut labels = vec![DiagnosticMessage::new(
                        format!(
                            "expression returns {} value{}",
                            expected.len(),
                            if expected.len() == 1 { "" } else { "s" }
                        ),
                        expression.source(),
                    )];

                    if let Expression::FunctionCall(function_call) = expression {
                        if let Some(return_types) = &function_call.return_types {
                            if return_types.len() > 0 {
                                labels.push(diag_return_types_label(return_types));
                            }
                        }
                    }

                    labels
                }),
            ),
            StatementError::AssignmentTypeMismatchError {
                ref expected_type,
                ref prev_var,
                ref var,
                assignment_source,
                ref expression,
            } => Self::new(&err, DiagnosticLevel::Error).with_context(
                DiagnosticContext::new(DiagnosticMessage::new(
                    format!("attempted to reassign variable as `{}` here", expected_type),
                    assignment_source,
                ))
                .with_labels({
                    let mut labels = vec![
                        diag_originally_defined(
                            prev_var.source(),
                            prev_var.get_type().map(|ty| ty.into()),
                        ),
                        diag_newly_defined(var.source(), var.get_type().map(|ty| ty.into())),
                    ];
                    if let Expression::FunctionCall(function_call) = expression {
                        labels.push(diag_return_types_label(
                            function_call.return_types.as_ref().expect(EXPECT_TYPES),
                        ));
                    }
                    labels
                }),
            ),
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
                        diag_return_types_label(&func_sig.returns),
                    ]),
                )
                .with_suggestions(vec![format!(
                    "If the result{} not needed, \
                    assign {} unused result to a discarded variable (\"_\").",
                    if func_sig.returns.len() == 1 {
                        " is"
                    } else {
                        "s are"
                    },
                    if func_sig.returns.len() == 1 {
                        "the"
                    } else {
                        "each"
                    },
                )]),
            StatementError::ReturnValueMismatchError {
                ref func_sig,
                ref return_types,
            } => Self::new(&err, DiagnosticLevel::Error).with_context(
                DiagnosticContext::new(diag_expected_types(&func_sig.returns, return_types))
                    .with_labels({
                        let mut labels = vec![diag_func_name_label(func_sig)];
                        if func_sig.returns.len() > 0 {
                            labels.push(diag_return_types_label(&func_sig.returns));
                        }
                        labels
                    }),
            ),
            StatementError::ExpressionError(err) => err.into(),
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
            let (expression_types, errs) = analyze_expression(expression, &scope);
            errors.append(
                &mut errs
                    .into_iter()
                    .map(|err| StatementError::ExpressionError(err))
                    .collect(),
            );

            if expression_types.len() != variables.len() {
                errors.push(StatementError::WrongNumberOfVariablesError {
                    expression: expression.clone(),
                    expected: expression_types,
                    actual: variables.clone(),
                });
            } else {
                for (i, variable) in variables.iter_mut().enumerate() {
                    match scope.get_var(variable.name()) {
                        Some(scope_var) => {
                            // Check if the variable has a different type than the expression result
                            if scope_var.get_type().expect(EXPECT_VAR_TYPE) != expression_types[i] {
                                errors.push(StatementError::AssignmentTypeMismatchError {
                                    expected_type: expression_types[i],
                                    prev_var: scope_var.clone(),
                                    var: variable.clone(),
                                    assignment_source: *source,
                                    expression: expression.clone(),
                                });
                            }

                            // Ensure that this variable is the same as variable already defined in scope
                            *variable = scope_var.clone();
                        }
                        // If the variable is not in scope, this is a new variable definition.
                        // Set the variable type to the corresponding expression result type and add it to the scope.
                        None => {
                            variable.set_type(&expression_types[i]);
                            if let Some(_) = scope.insert_var(variable.clone()) {
                                unreachable!("variable cannot already be defined");
                            }
                        }
                    }
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
            if func_sig.returns != return_types {
                errors.push(StatementError::ReturnValueMismatchError {
                    func_sig: func_sig.clone(),
                    return_types, // TODO: FIX THIS
                })
            }
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

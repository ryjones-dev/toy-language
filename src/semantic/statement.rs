use thiserror::Error;

use crate::{
    diagnostic::{Diagnostic, DiagnosticContext, DiagnosticLevel, DiagnosticMessage},
    parser::{
        expression::Expression,
        source_range::SourceRange,
        statement::Statement,
        types::{Type, Types},
        variable::{Variable, Variables},
    },
};

use super::{
    diagnostic::{
        diag_expected, diag_newly_defined, diag_originally_defined, diag_return_types_label,
    },
    expression::{analyze_expression, ExpressionError},
    scope_tracker::ScopeTracker,
    EXPECT_FUNC_SIG, EXPECT_VAR_TYPE,
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
        var: Variable,
        assignment_source: SourceRange,
        expression: Expression,
    },
    #[error("variable type redefinition")]
    VariableTypeRedefinitionError { prev_var: Variable, var: Variable },
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

                    if let Expression::FunctionCall {
                        function_signature, ..
                    } = expression
                    {
                        if let Some(label) = diag_return_types_label(
                            function_signature.as_ref().expect(EXPECT_FUNC_SIG),
                        ) {
                            labels.push(label);
                        }
                    }

                    labels
                }),
            ),
            StatementError::AssignmentTypeMismatchError {
                ref expected_type,
                ref var,
                assignment_source,
                ref expression,
            } => Self::new(&err, DiagnosticLevel::Error).with_context(
                DiagnosticContext::new(DiagnosticMessage::new(
                    format!(
                        "attempted to assign result of type `{}` to variable of type `{}`",
                        expected_type,
                        var.get_type().expect(EXPECT_VAR_TYPE)
                    ),
                    assignment_source,
                ))
                .with_labels({
                    let mut labels = vec![DiagnosticMessage::new(
                        "variable type defined here",
                        var.get_type().as_ref().expect(EXPECT_VAR_TYPE).source(),
                    )];
                    if let Expression::FunctionCall {
                        function_signature, ..
                    } = expression
                    {
                        if let Some(label) = diag_return_types_label(
                            function_signature.as_ref().expect(EXPECT_FUNC_SIG),
                        ) {
                            labels.push(label);
                        }
                    }
                    labels
                }),
            ),
            StatementError::VariableTypeRedefinitionError {
                ref prev_var,
                ref var,
            } => Self::new(&err, DiagnosticLevel::Error).with_context(
                DiagnosticContext::new(diag_expected(
                    prev_var.get_type().as_ref().expect(EXPECT_VAR_TYPE),
                    var.get_type().as_ref().expect(EXPECT_VAR_TYPE),
                    var.source(),
                ))
                .with_labels(vec![
                    diag_originally_defined(
                        prev_var.source(),
                        prev_var.get_type().map(|ty| ty.into()),
                    ),
                    diag_newly_defined(var.source(), var.get_type().map(|ty| ty.into())),
                ]),
            ),
            StatementError::ExpressionError(err) => err.into(),
        }
    }
}

#[derive(Debug)]
pub(super) enum ReturnResults {
    /// The enclosing scope should return the given types.
    ///
    /// The statement is optional since no return expression means no return types.
    ConvergentReturn(Option<Statement>, Types),
    /// The enclosing scope is divergent, meaning that it should return the given types as the result
    /// of an outer function.
    ///
    /// The statement is not optional because scopes cannot diverge without a function return expression.
    DivergentReturn(Statement, Types),
}

impl Default for ReturnResults {
    fn default() -> Self {
        ReturnResults::ConvergentReturn(None, Types::default())
    }
}

/// Returns the types that statement returns, if any, and any errors the statement produced.
pub(super) fn analyze_statement(
    statement: &mut Statement,
    scope_tracker: &mut ScopeTracker,
) -> (Option<ReturnResults>, Vec<StatementError>) {
    let mut errors = Vec::new();
    match statement {
        Statement::Assignment {
            variables,
            expression,
            source,
        } => {
            let (expression_types, mut errs) = get_expression_types(expression, scope_tracker);
            errors.append(&mut errs);

            // Only continue if the expression did not have errors
            if errors.len() == 0 {
                if expression_types.len() != variables.len() {
                    errors.push(StatementError::WrongNumberOfVariablesError {
                        expression: expression.clone(),
                        expected: expression_types,
                        actual: variables.clone(),
                    });
                } else {
                    for (i, variable) in variables.iter_mut().enumerate() {
                        if let Some(var_type) = variable.get_type() {
                            // Check if the expression result matches the variable's annotated type
                            if *var_type != expression_types[i] {
                                errors.push(StatementError::AssignmentTypeMismatchError {
                                    expected_type: expression_types[i],
                                    var: variable.clone(),
                                    assignment_source: *source,
                                    expression: expression.clone(),
                                });
                            }
                        } else {
                            // Variable has not been explicitly assigned a type,
                            // so give it the same type as the expression result
                            variable.set_type(&expression_types[i]);
                        }

                        // Add the variable to the scope
                        if let Some(scope_var) = scope_tracker.insert_var(variable.clone()) {
                            // The variable has already been added to the scope, so make sure it has the same type
                            if scope_var.get_type() != variable.get_type() {
                                errors.push(StatementError::VariableTypeRedefinitionError {
                                    prev_var: scope_var.clone(),
                                    var: variable.clone(),
                                });
                            }
                        }
                    }
                }
            }

            (None, errors)
        }
        Statement::ScopeReturn { expressions, .. } => {
            let (return_types, mut errs) = get_expressions_types(expressions, scope_tracker);
            errors.append(&mut errs);
            (
                Some(ReturnResults::ConvergentReturn(
                    Some(statement.clone()),
                    return_types,
                )),
                errors,
            )
        }
        Statement::FunctionReturn { expressions, .. } => {
            let (return_types, mut errs) = get_expressions_types(expressions, scope_tracker);
            errors.append(&mut errs);
            (
                Some(ReturnResults::DivergentReturn(
                    statement.clone(),
                    return_types,
                )),
                errors,
            )
        }
    }
}

fn get_expression_types(
    expression: &mut Expression,
    scope_tracker: &ScopeTracker,
) -> (Types, Vec<StatementError>) {
    let (types, errs) = analyze_expression(expression, &scope_tracker);
    let errs = errs
        .into_iter()
        .map(|err| StatementError::ExpressionError(err))
        .collect();

    (types, errs)
}

fn get_expressions_types(
    expressions: &mut Vec<Expression>,
    scope_tracker: &ScopeTracker,
) -> (Types, Vec<StatementError>) {
    let mut types = Types::new();
    let mut errors = Vec::new();

    for expression in expressions {
        let (mut tys, mut errs) = get_expression_types(expression, scope_tracker);
        types.append(&mut tys);
        errors.append(&mut errs);
    }

    (types, errors)
}

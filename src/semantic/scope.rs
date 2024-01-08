use thiserror::Error;

use crate::{
    diagnostic::{Diagnostic, DiagnosticContext, DiagnosticLevel, DiagnosticMessage},
    parser::{
        expression::Expression, function::FunctionSignature, scope::Scope,
        source_range::SourceRange, types::Types, variable::Variable,
    },
    semantic::diagnostic::diag_return_types_label,
};

use super::{
    expression::{analyze_expression, ExpressionError, ExpressionResult},
    scope_tracker::ScopeTracker,
};

#[derive(Debug, Error)]
pub(super) enum ScopeError {
    #[error("return before end of scope")]
    EarlyReturnError {
        return_expression: Expression,
        remaining_code_source: SourceRange,
    },
    #[error("unassigned return value{}", if .types.len() == 1 { "" } else { "s" })]
    NonZeroReturnError {
        expression: Expression,
        types: Types,
        func_sig: Option<FunctionSignature>,
    },
    #[error("unused variable")]
    UnusedVariableError { variable: Variable },
    #[error("unused function")]
    UnusedFunctionError {
        function_signature: FunctionSignature,
    },
    #[error(transparent)]
    ExpressionError(#[from] ExpressionError),
}

impl From<ScopeError> for Diagnostic {
    fn from(err: ScopeError) -> Self {
        match err {
            ScopeError::EarlyReturnError {
                ref return_expression,
                remaining_code_source,
            } => Self::new(&err, DiagnosticLevel::Error).with_context(
                DiagnosticContext::new(DiagnosticMessage::new(
                    "there is more code below this return",
                    return_expression.source(),
                ))
                .with_labels(vec![DiagnosticMessage::new(
                    "unreachable code",
                    remaining_code_source,
                )]),
            ),
            ScopeError::NonZeroReturnError {
                ref expression,
                ref types,
                ref func_sig,
            } => Self::new(&err, DiagnosticLevel::Error)
                .with_context(
                    DiagnosticContext::new(DiagnosticMessage::new(
                        if types.len() == 1 {
                            "result of expression has not been assigned"
                        } else {
                            "results of expression have not been assigned"
                        },
                        expression.source(),
                    ))
                    .with_labels(if let Some(func_sig) = func_sig {
                        if let Some(msg) = diag_return_types_label(func_sig) {
                            vec![msg]
                        } else {
                            vec![]
                        }
                    } else {
                        vec![]
                    }),
                )
                .with_suggestions({
                    let mut msg = if types.len() == 1 {
                        "If the result of the expression is not needed, \
                    assign it to discarded variable."
                    } else {
                        "If the results of the expression are not needed, \
                    assign them to discarded variables."
                    }
                    .to_string();
                    let mut discards = "_, ".repeat(types.len());
                    discards.truncate(discards.len() - 2);
                    discards.push_str(" = ...");
                    msg.push_str(&format!(" `{discards}`"));
                    vec![msg]
                }),
            ScopeError::UnusedVariableError { ref variable } => {
                Self::new(&err, DiagnosticLevel::Warning)
                    .with_context(DiagnosticContext::new(DiagnosticMessage::new(
                        format!("variable `{}` is never read", variable),
                        variable.source(),
                    )))
                    .with_suggestions(vec![
                    "Either remove the variable, or prefix it with an underscore to discard it.",
                ])
            }
            ScopeError::UnusedFunctionError {
                ref function_signature,
            } => Self::new(&err, DiagnosticLevel::Warning)
                .with_context(DiagnosticContext::new(DiagnosticMessage::new(
                    format!("function `{}` is never called", function_signature.name),
                    function_signature.source,
                )))
                .with_suggestions(vec![
                    "Either remove the function, or prefix it with an underscore to discard it.",
                ]),
            ScopeError::ExpressionError(err) => err.into(),
        }
    }
}

pub(super) fn analyze_scope(
    scope: &mut Scope,
    mut scope_tracker: ScopeTracker,
) -> (ExpressionResult, Vec<ScopeError>) {
    let mut early_return_index = None;
    let mut errors = Vec::new();

    // if this is an empty scope, don't bother trying to analyze anything
    if scope.len() == 0 {
        return (ExpressionResult::Return(Types::new()), errors);
    }

    // Throw out the scope's return expression from the rest of the body.
    // This simplifies the logic for analysis.
    let (body, _) = scope.split_return_mut();

    for (i, expression) in body.iter_mut().enumerate() {
        let (result, errs) = analyze_expression(expression, &mut scope_tracker);
        errors.append(
            &mut errs
                .into_iter()
                .map(|err| ScopeError::ExpressionError(err))
                .collect(),
        );

        match result {
            ExpressionResult::Return(types) => {
                if types.len() > 0 {
                    // It is an error if any expression in the scope body returned types
                    errors.push(ScopeError::NonZeroReturnError {
                        expression: expression.clone(),
                        types,
                        func_sig: match expression {
                            Expression::FunctionCall {
                                function_signature, ..
                            } => function_signature.clone(),
                            _ => None,
                        },
                    })
                }
            }
            ExpressionResult::DivergentReturn(_) => {
                // Scope has more expressions after the divergent return.
                // We can't generate an error here because the scope body is mutably borrowed during this loop.
                // Instead, cache the index and generate the error for the first occurrence after iteration is done.
                if let None = early_return_index {
                    early_return_index = Some(i)
                }
            }
        }
    }

    // If we detected an early divergent return, generate that error now
    if let Some(i) = early_return_index {
        let expression = scope[i].clone();
        let remaining = &scope[i + 1..];
        errors.push(ScopeError::EarlyReturnError {
            return_expression: expression.clone(),
            remaining_code_source: remaining
                .first()
                .unwrap()
                .source()
                .combine(remaining.last().unwrap().source()),
        });
    }

    // Now we can get the last return expression and handle that separately.
    // We can't get both in the same call because we need to access the whole
    // scope again when generating an early return error.
    let (_, returns) = scope.split_return_mut();

    // Analyze the scope's return expressions
    let (result, errs) = analyze_expression(returns, &mut scope_tracker);
    errors.append(
        &mut errs
            .into_iter()
            .map(|err| ScopeError::ExpressionError(err))
            .collect(),
    );

    // Check for any variables or functions in the immediate scope that have not been used
    let (unused_variables, function_signatures) = scope_tracker.get_unused();
    for variable in unused_variables {
        errors.push(ScopeError::UnusedVariableError { variable })
    }
    for func_sig in function_signatures {
        errors.push(ScopeError::UnusedFunctionError {
            function_signature: func_sig,
        })
    }

    (result, errors)
}

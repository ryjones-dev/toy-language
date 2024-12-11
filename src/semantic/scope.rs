use thiserror::Error;

use crate::{
    diagnostic::{Diagnostic, DiagnosticContext, DiagnosticLevel, DiagnosticMessage},
    parser::{
        expression::Expression, function::FunctionSignature, r#struct::Struct, scope::Scope,
        source_range::SourceRange, types::Types, variable::Variable,
    },
    semantic::diagnostic::diag_return_types_label,
};

use super::{
    expression::{analyze_expression, ExpressionError},
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
    #[error("unused struct")]
    UnusedStructError { _struct: Struct },
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
            ScopeError::UnusedStructError { ref _struct } => {
                Self::new(&err, DiagnosticLevel::Warning)
                    .with_context(DiagnosticContext::new(DiagnosticMessage::new(
                        format!("struct `{}` is never instantiated", _struct),
                        _struct.source(),
                    )))
                    .with_suggestions(vec![
                        "Either remove the struct, or prefix it with an underscore to discard it.",
                    ])
            }
            ScopeError::ExpressionError(err) => err.into(),
        }
    }
}

pub(super) fn analyze_scope(
    scope: &mut Scope,
    mut scope_tracker: ScopeTracker,
    outer_func_sig: &FunctionSignature,
) -> (Types, Vec<ScopeError>) {
    let mut early_return_index = None;
    let mut types = Types::new();
    let mut errors = Vec::new();

    // Split out the scope's return expression from the rest of the body.
    // This simplifies the logic for analysis.
    if let Some((returns, body)) = scope.split_return_mut() {
        for (i, expression) in body.iter_mut().enumerate() {
            let (types, errs) = analyze_expression(expression, &mut scope_tracker, outer_func_sig);
            errors.append(
                &mut errs
                    .into_iter()
                    .map(|err| ScopeError::ExpressionError(err))
                    .collect(),
            );

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

            match expression {
                Expression::FunctionReturn { .. } => {
                    // Scope has more expressions after the function return.
                    // We can't generate an error here because the scope is mutably borrowed from splitting out
                    // the return expression from the rest of the body.
                    // Instead, cache the index and generate the error for the first occurrence after iteration is done.
                    if let None = early_return_index {
                        early_return_index = Some(i)
                    }
                }
                _ => {}
            }
        }

        // Analyze the scope's return expressions
        let (tys, errs) = analyze_expression(returns, &mut scope_tracker, outer_func_sig);
        types = tys;
        errors.append(
            &mut errs
                .into_iter()
                .map(|err| ScopeError::ExpressionError(err))
                .collect(),
        );
    }

    // If we detected an early function return in the scope body, generate that error now
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

    // Check for any variables, functions, or structs in the immediate scope that have not been used
    let (unused_variables, function_signatures, structs) = scope_tracker.get_unused();
    for variable in unused_variables {
        errors.push(ScopeError::UnusedVariableError { variable })
    }
    for func_sig in function_signatures {
        errors.push(ScopeError::UnusedFunctionError {
            function_signature: func_sig,
        })
    }
    for _struct in structs {
        errors.push(ScopeError::UnusedStructError { _struct })
    }

    (types, errors)
}

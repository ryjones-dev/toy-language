use thiserror::Error;

use crate::{
    diagnostic::{Diagnostic, DiagnosticContext, DiagnosticLevel, DiagnosticMessage},
    parser::{
        function::FunctionSignature,
        scope::Scope,
        source_range::SourceRange,
        statement::Statement,
        types::Types,
        variable::{Variable, Variables},
    },
};

use super::{
    diagnostic::{diag_newly_defined, diag_originally_defined},
    scope_tracker::ScopeTracker,
    statement::{analyze_statement, ReturnResults, StatementError},
};

#[derive(Debug, Error)]
pub(super) enum ScopeError {
    #[error("duplicate function parameter")]
    DuplicateParameterError { original: Variable, new: Variable },
    #[error("return before end of scope")]
    EarlyReturnError {
        return_statement: Statement,
        expected_types: Types,
        remaining_code_source: SourceRange,
    },
    #[error("unused variable")]
    UnusedVariableError { variable: Variable },
    #[error("unused function")]
    UnusedFunctionError {
        function_signature: FunctionSignature,
    },
    #[error(transparent)]
    StatementError(#[from] StatementError),
}

impl From<ScopeError> for Diagnostic {
    fn from(err: ScopeError) -> Self {
        match err {
            ScopeError::DuplicateParameterError {
                ref original,
                ref new,
            } => Self::new(&err, DiagnosticLevel::Error).with_context(
                DiagnosticContext::new(DiagnosticMessage::new(
                    format!("scope already has a parameter named `{}`", new.name()),
                    new.source(),
                ))
                .with_labels(vec![
                    diag_originally_defined(original.source(), None),
                    diag_newly_defined(new.source(), None),
                ]),
            ),
            ScopeError::EarlyReturnError {
                ref return_statement,
                ref expected_types,
                remaining_code_source,
            } => Self::new(&err, DiagnosticLevel::Error)
                .with_context(
                    DiagnosticContext::new(DiagnosticMessage::new(
                        "there is more code below this return statement",
                        return_statement.source(),
                    ))
                    .with_labels(vec![DiagnosticMessage::new(
                        "unreachable code",
                        remaining_code_source,
                    )]),
                )
                .with_suggestions({
                    if expected_types.len() > 0 {
                        vec![format!(
                            "If returning from the scope is not intentional, \
                                assign the result{} to {}discarded variable{} instead.",
                            if expected_types.len() == 1 { "" } else { "s" },
                            if expected_types.len() == 1 { "a " } else { "" },
                            if expected_types.len() == 1 { "" } else { "s" }
                        )]
                    } else {
                        Vec::new() // TODO: How do we handle a function call that returns nothing, but is not meant to be a return statement?
                    }
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
            ScopeError::StatementError(err) => err.into(),
        }
    }
}

pub(super) fn analyze_scope(
    scope: &mut Scope,
    params: &Variables,
    outer_scope_tracker: &ScopeTracker,
) -> (ReturnResults, Vec<ScopeError>) {
    let mut return_results = None;
    let mut errors = Vec::new();

    let mut scope_tracker = ScopeTracker::new(Some(outer_scope_tracker));

    // Add the scope parameters to the scope
    for param in params {
        if let Some(variable) = scope_tracker.insert_var(param.clone().into()) {
            errors.push(ScopeError::DuplicateParameterError {
                original: variable.clone(),
                new: param.clone(),
            });
        }
    }

    let num_statements = scope.num_statements();
    let mut first_return_index = None;
    for (i, statement) in scope.iter_mut().enumerate() {
        let (results, errs) = analyze_statement(statement, &mut scope_tracker);
        errors.append(
            &mut errs
                .into_iter()
                .map(|err| ScopeError::StatementError(err))
                .collect(),
        );

        if let Some(results) = results {
            if let None = return_results {
                return_results = Some(results);

                if i != num_statements - 1 {
                    // Scope has more statements after the return.
                    // We can't generate an error here because the statement list is mutably borrowed
                    // during this loop.
                    // Instead, keep track of the index and generate the error after iteration is done.
                    first_return_index = Some(i);
                }
            }
        }
    }

    // If we detected an early return, generate that error now
    if let Some(i) = first_return_index {
        let statement = scope[i].clone();
        let remaining = &scope[i + 1..];
        errors.push(ScopeError::EarlyReturnError {
            return_statement: statement.clone(),
            expected_types: match return_results.as_ref().unwrap() {
                ReturnResults::ConvergentReturn(_, types) => types.clone(),
                ReturnResults::DivergentReturn(_, types) => types.clone(),
            },
            remaining_code_source: remaining
                .first()
                .unwrap()
                .source()
                .combine(remaining.last().unwrap().source()),
        });
    }

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

    (return_results.unwrap_or_default(), errors)
}

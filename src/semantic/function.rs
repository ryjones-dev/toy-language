use thiserror::Error;

use crate::{
    diagnostic::{Diagnostic, DiagnosticContext, DiagnosticLevel, DiagnosticMessage},
    parser::{
        expression::Expression,
        function::{Function, FunctionSignature},
        variable::Variable,
    },
    semantic::scope::analyze_scope,
};

use super::{
    diagnostic::{diag_func_name_label, diag_newly_defined, diag_originally_defined},
    expression::ExpressionError,
    scope::ScopeError,
    scope_tracker::ScopeTracker,
};

#[derive(Debug, Error)]
pub(super) enum FunctionError {
    #[error("duplicate function parameter")]
    DuplicateParameterError {
        func_sig: FunctionSignature,
        original: Variable,
        new: Variable,
    },
    #[error(transparent)]
    ScopeError(#[from] ScopeError),
}

impl From<FunctionError> for Diagnostic {
    fn from(err: FunctionError) -> Self {
        match err {
            FunctionError::DuplicateParameterError {
                ref func_sig,
                ref original,
                ref new,
            } => Self::new(&err, DiagnosticLevel::Error).with_context(
                DiagnosticContext::new(DiagnosticMessage::new(
                    format!(
                        "function `{}` already has a parameter named `{}`",
                        func_sig.name,
                        new.name()
                    ),
                    new.source(),
                ))
                .with_labels(vec![
                    diag_func_name_label(func_sig),
                    diag_originally_defined(original.source(), None),
                    diag_newly_defined(new.source(), None),
                ]),
            ),
            FunctionError::ScopeError(err) => err.into(),
        }
    }
}

pub(super) fn analyze_function(
    function: &mut Function,
    outer_scope_tracker: &ScopeTracker,
) -> Vec<FunctionError> {
    let mut errors = Vec::new();

    let mut scope_tracker = ScopeTracker::new(Some(outer_scope_tracker));

    // Add the function parameters to the scope
    for param in &function.signature.params {
        if let Some(variable) = scope_tracker.insert_var(param.clone().into()) {
            errors.push(FunctionError::DuplicateParameterError {
                func_sig: function.signature.clone(),
                original: variable.clone(),
                new: param.clone(),
            });
        }
    }

    let (types, errs) = analyze_scope(&mut function.scope, scope_tracker, &function.signature);
    errors.append(
        &mut errs
            .into_iter()
            .map(|err| FunctionError::ScopeError(err))
            .collect(),
    );

    // Ensure that the function returns the correct types
    if function.signature.returns != types {
        if let Some((returns, _)) = function.scope.split_return() {
            let returns = returns.unwrap_transparent();
            match returns {
                // Special case: if a function return is explicitly used at the end of a scope,
                // it won't be returned from the scope analysis.
                // In that case, we don't need to do anything, because the error was already checked for.
                Expression::FunctionReturn { .. } => {}
                _ => {
                    errors.push(FunctionError::ScopeError(ScopeError::ExpressionError(
                        ExpressionError::FunctionReturnValueMismatchError {
                            func_sig: function.signature.clone(),
                            return_types: types,
                            source_range: Some(returns.source()),
                        },
                    )));
                }
            }
        } else {
            errors.push(FunctionError::ScopeError(ScopeError::ExpressionError(
                ExpressionError::FunctionReturnValueMismatchError {
                    func_sig: function.signature.clone(),
                    return_types: types,
                    source_range: None,
                },
            )));
        }
    }

    // Ensure the function scope has a function return at the end to simplify codegen
    function.scope.wrap_function_return();

    errors
}

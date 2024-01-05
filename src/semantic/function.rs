use thiserror::Error;

use crate::{
    diagnostic::{Diagnostic, DiagnosticContext, DiagnosticLevel},
    parser::{
        function::{Function, FunctionSignature},
        statement::Statement,
        types::Types,
    },
    semantic::scope::analyze_scope,
};

use super::{
    diagnostic::{
        diag_expected, diag_expected_types, diag_func_name_label, diag_return_types_label,
    },
    scope::ScopeError,
    scope_tracker::ScopeTracker,
    statement::ReturnResults,
};

#[derive(Debug, Error)]
pub(super) enum FunctionError {
    #[error("return value mismatch")]
    ReturnValueMismatchError {
        func_sig: FunctionSignature,
        statement: Option<Statement>,
        return_types: Types,
    },
    #[error(transparent)]
    ScopeError(#[from] ScopeError),
}

impl From<FunctionError> for Diagnostic {
    fn from(err: FunctionError) -> Self {
        match err {
            FunctionError::ReturnValueMismatchError {
                ref func_sig,
                ref statement,
                ref return_types,
            } => Self::new(&err, DiagnosticLevel::Error).with_context(
                DiagnosticContext::new({
                    if let Some(statement) = statement {
                        diag_expected(&func_sig.returns, return_types, statement.source())
                    } else {
                        diag_expected_types(&func_sig.returns, &Types::new())
                    }
                })
                .with_labels({
                    let mut labels = vec![diag_func_name_label(func_sig)];
                    if let Some(label) = diag_return_types_label(&func_sig) {
                        labels.push(label);
                    }
                    labels
                }),
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

    let (results, errs) = analyze_scope(
        &mut function.body,
        &function.signature.params,
        outer_scope_tracker,
    );
    errors.append(
        &mut errs
            .into_iter()
            .map(|err| FunctionError::ScopeError(err))
            .collect(),
    );

    // Only continue if there were no statement errors
    if errors.len() == 0 {
        // It doesn't matter whether the scope returned convergently or divergently for a function scope
        let (statement, types) = match results {
            ReturnResults::ConvergentReturn(statement, types) => (statement, types),
            ReturnResults::DivergentReturn(statement, types) => (Some(statement), types),
        };

        // Ensure that the function returns the correct types
        if function.signature.returns != types {
            errors.push(FunctionError::ReturnValueMismatchError {
                func_sig: function.signature.clone(),
                statement,
                return_types: types,
            });
        }
    }

    errors
}

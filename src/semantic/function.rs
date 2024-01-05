use thiserror::Error;

use crate::{
    diagnostic::{Diagnostic, DiagnosticContext, DiagnosticLevel, DiagnosticMessage},
    parser::{
        function::{Function, FunctionSignature},
        statement::Statement,
        types::Types,
        variable::Variable,
    },
    semantic::statement::analyze_statements,
};

use super::{
    diagnostic::{
        diag_expected, diag_expected_types, diag_func_name_label, diag_newly_defined,
        diag_originally_defined, diag_return_types_label,
    },
    scope_tracker::ScopeTracker,
    statement::{ScopeResults, StatementsError},
};

#[derive(Debug, Error)]
pub(super) enum FunctionError {
    #[error("duplicate function parameter")]
    DuplicateParameterError {
        func_sig: FunctionSignature,
        original: Variable,
        new: Variable,
    },
    #[error("return value mismatch")]
    ReturnValueMismatchError {
        func_sig: FunctionSignature,
        statement: Option<Statement>,
        return_types: Types,
    },
    #[error(transparent)]
    StatementsError(#[from] StatementsError),
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
                    diag_func_name_label(&func_sig),
                    diag_originally_defined(original.source(), None),
                    diag_newly_defined(new.source(), None),
                ]),
            ),
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
            FunctionError::StatementsError(err) => err.into(),
        }
    }
}

pub(super) fn analyze_function(
    function: &mut Function,
    outer_scope_tracker: &mut ScopeTracker,
) -> Vec<FunctionError> {
    let mut errors = Vec::new();

    let mut scope_tracker = ScopeTracker::new(Some(&outer_scope_tracker));

    // Insert parameters into function scope
    for param in &function.signature.params {
        if let Some(variable) = scope_tracker.insert_var(param.clone().into()) {
            errors.push(FunctionError::DuplicateParameterError {
                func_sig: function.signature.clone(),
                original: variable.clone(),
                new: param.clone(),
            });
        }
    }

    let (results, errs) = analyze_statements(&mut function.body, scope_tracker);
    errors.append(
        &mut errs
            .into_iter()
            .map(|err| FunctionError::StatementsError(err))
            .collect(),
    );

    // Only continue if there were no statement errors
    if errors.len() == 0 {
        // It doesn't matter whether the scope returned convergently or divergently for a function scope
        let (statement, types) = match results {
            ScopeResults::ConvergentReturn(statement, types) => (statement, types),
            ScopeResults::DivergentReturn(statement, types) => (Some(statement), types),
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

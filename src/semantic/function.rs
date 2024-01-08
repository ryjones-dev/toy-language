use thiserror::Error;

use crate::{
    diagnostic::{Diagnostic, DiagnosticContext, DiagnosticLevel, DiagnosticMessage},
    parser::{
        function::{Function, FunctionSignature},
        source_range::SourceRange,
        types::Types,
        variable::Variable,
    },
    semantic::scope::analyze_scope,
};

use super::{
    diagnostic::{
        diag_expected, diag_expected_types, diag_func_name_label, diag_newly_defined,
        diag_originally_defined, diag_return_types_label,
    },
    expression::ExpressionResult,
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
    #[error("return value mismatch")]
    ReturnValueMismatchError {
        func_sig: FunctionSignature,
        return_types: Types,
        source_range: Option<SourceRange>,
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
            FunctionError::ReturnValueMismatchError {
                ref func_sig,
                ref return_types,
                ref source_range,
            } => Self::new(&err, DiagnosticLevel::Error).with_context(
                DiagnosticContext::new({
                    if let Some(source_range) = source_range {
                        diag_expected(&func_sig.returns, return_types, *source_range)
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

    let mut scope_tracker = ScopeTracker::new(Some(outer_scope_tracker));

    // Add the scope parameters to the scope
    for param in &function.signature.params {
        if let Some(variable) = scope_tracker.insert_var(param.clone().into()) {
            errors.push(FunctionError::DuplicateParameterError {
                func_sig: function.signature.clone(),
                original: variable.clone(),
                new: param.clone(),
            });
        }
    }

    let (result, errs) = analyze_scope(&mut function.scope, scope_tracker);
    errors.append(
        &mut errs
            .into_iter()
            .map(|err| FunctionError::ScopeError(err))
            .collect(),
    );

    let types = result.types();

    // Ensure that the function returns the correct types
    if function.signature.returns != types {
        errors.push(FunctionError::ReturnValueMismatchError {
            func_sig: function.signature.clone(),
            return_types: types,
            source_range: if function.scope.len() > 0 {
                Some(function.scope.split_return_mut().1.source())
            } else {
                None
            },
        });
    }

    // Ensure the function scope has a divergent return to simplify codegen
    match result {
        ExpressionResult::Return(_) => function.scope.wrap_divergent_return(),
        ExpressionResult::DivergentReturn(_) => {}
    }

    errors
}

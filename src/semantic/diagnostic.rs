//! Helper functions for creating common [`DiagnosticMessage`]s.
use crate::{
    diagnostic::DiagnosticMessage,
    parser::{function::FunctionSignature, types::Types},
};

/// Creates a new [`DiagnosticMessage`] comparing lists of types.
///
/// The returned [`DiagnosticMessage`] will have the source range from `actual`,
/// unless `actual` is empty in which case it will have the source range from `expected`.
///
/// # Panics
/// Panics if `expected` and `actual` are both empty.
pub(super) fn diag_expected_types(expected: &Types, actual: &Types) -> DiagnosticMessage {
    DiagnosticMessage::new(
        format!("expected {}, found {}", expected, actual),
        if actual.len() > 0 {
            actual.source().unwrap()
        } else if expected.len() > 0 {
            expected.source().unwrap()
        } else {
            panic!("no types to get a source range from")
        },
    )
}

/// Creates a new [`DiagnosticMessage`] labeling the function's name.
pub(super) fn diag_func_name_label(func_sig: &FunctionSignature) -> DiagnosticMessage {
    DiagnosticMessage::new(
        format!("for function `{}`", func_sig.name),
        func_sig.name.source(),
    )
}

/// Creates a new [`DiagnosticMessage`] labeling a function signature's return types.
///
/// # Panics
/// Panics if the function signature has no return types.
pub(super) fn diag_func_sig_return_label(func_sig: &FunctionSignature) -> DiagnosticMessage {
    DiagnosticMessage::new(
        "return types defined here",
        func_sig
            .returns
            .source()
            .expect("function signature should have return types for this `DiagnosticMessage`"),
    )
}

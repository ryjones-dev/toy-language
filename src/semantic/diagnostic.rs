//! Helper functions for creating common [`DiagnosticMessage`]s.
use crate::{
    diagnostic::DiagnosticMessage,
    parser::{function::FunctionSignature, source_range::SourceRange},
};

/// Creates a new [`DiagnosticMessage`] with an expected value and an actual value of the same type.
pub(super) fn diag_expected_actual<T: std::fmt::Display>(
    expected: &T,
    actual: &T,
    source: SourceRange,
) -> DiagnosticMessage {
    DiagnosticMessage::new(format!("expected {}, found {}", expected, actual), source)
}

/// Creates a new [`DiagnosticMessage`] labeling the entire function signature.
pub(super) fn diag_func_sig_label(func_sig: &FunctionSignature) -> DiagnosticMessage {
    DiagnosticMessage::new(format!("for function `{}`", func_sig.name), func_sig.source)
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

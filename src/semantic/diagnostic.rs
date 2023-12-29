//! Helper functions for creating common [`DiagnosticMessage`]s.
use crate::{
    diagnostic::DiagnosticMessage,
    parser::{
        function::{FunctionParameters, FunctionSignature},
        source_range::SourceRange,
        types::{DataType, Types},
    },
};

/// Creates a new [`DiagnosticMessage`] comparing two types, and a given [`SourceRange`].
pub(super) fn diag_expected<T: std::fmt::Display>(
    expected: &T,
    actual: &T,
    source: SourceRange,
) -> DiagnosticMessage {
    DiagnosticMessage::new(format!("expected {}, found {}", expected, actual), source)
}

/// Creates a new [`DiagnosticMessage`] comparing two lists of types.
///
/// The returned [`DiagnosticMessage`] will have the source range from `actual`,
/// unless `actual` is empty in which case it will have the source range from `expected`.
///
/// # Panics
/// Panics if `expected` and `actual` are both empty.
pub(super) fn diag_expected_types(expected: &Types, actual: &Types) -> DiagnosticMessage {
    diag_expected(
        expected,
        actual,
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

/// Creates a new [`DiagnosticMessage`] labeling the function's parameters.
///
/// # Panics
/// Panics if [`FunctionParameters`] is empty.
pub(super) fn diag_func_param_label(params: &FunctionParameters) -> DiagnosticMessage {
    DiagnosticMessage::new(
        format!(
            "function parameter{} defined here",
            if params.len() == 1 { "" } else { "s" }
        ),
        params
            .source()
            .expect("should have parameter types for this `DiagnosticMessage`"),
    )
}

/// Creates a new [`DiagnosticMessage`] labeling return types.
///
/// # Panics
/// Panics if [`Types`] is empty.
pub(super) fn diag_return_types_label(return_types: &Types) -> DiagnosticMessage {
    DiagnosticMessage::new(
        format!(
            "return type{} defined here",
            if return_types.len() == 1 { "" } else { "s" }
        ),
        return_types
            .source()
            .expect("should have return types for this `DiagnosticMessage`"),
    )
}

/// Creates a new [`DiagnosticMessage`] labeling where a subject is originally defined.
/// Provide a [`DataType`] to specify which type the subject is defined with.
pub(super) fn diag_originally_defined(
    source: SourceRange,
    ty: Option<DataType>,
) -> DiagnosticMessage {
    DiagnosticMessage::new(
        format!(
            "originally defined here{}",
            if let Some(ty) = ty {
                format!(" with type `{}`", ty)
            } else {
                String::new()
            }
        ),
        source,
    )
}

/// Creates a new [`DiagnosticMessage`] labeling where a subject is newly defined.
/// Provide a [`DataType`] to specify which type the subject is defined with.
pub(super) fn diag_newly_defined(source: SourceRange, ty: Option<DataType>) -> DiagnosticMessage {
    DiagnosticMessage::new(
        format!(
            "newly defined here{}",
            if let Some(ty) = ty {
                format!(" with type `{}`", ty)
            } else {
                String::new()
            }
        ),
        source,
    )
}

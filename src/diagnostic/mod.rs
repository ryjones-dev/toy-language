use std::error::Error;

use codespan_reporting::{files::SimpleFiles, term::termcolor::StandardStream};
use thiserror::Error;

use crate::parser::source_range::SourceRange;

/// An error that is returned when the compiler is unable to render error messages to stderr.
#[derive(Debug, Error)]
#[error("failed to render error message: {0}")]
pub struct RenderErrorFailure(codespan_reporting::files::Error);

/// Contains a to message, label, or note to display as part of a [`DiagnosticContext`].
///
/// The [`DiagnosticMessage`] contains a [`SourceRange`] so that the diagnostic message can render
/// as annotations in a source code preview.
pub(crate) struct DiagnosticMessage {
    message: String,
    source: SourceRange,
}

impl DiagnosticMessage {
    pub(crate) fn new(message: impl Into<String>, source: SourceRange) -> Self {
        Self {
            message: message.into(),
            source,
        }
    }

    fn to_primary<F>(self, file_id: F) -> codespan_reporting::diagnostic::Label<F> {
        codespan_reporting::diagnostic::Label::primary(file_id, Self::to_range(self.source))
            .with_message(self.message)
    }

    fn to_secondary<F>(self, file_id: F) -> codespan_reporting::diagnostic::Label<F> {
        codespan_reporting::diagnostic::Label::secondary(file_id, Self::to_range(self.source))
            .with_message(self.message)
    }

    /// Gross conversion from [`SourceRange`] -> [`std::ops::RangeInclusive<usize>`] -> [`std::ops::Range<usize>`].
    ///
    /// [`codespan_reporting`] only accepts a [`std::ops::Range<usize>`] for some reason, even though it is operating on indices
    /// and [`std::ops::RangeInclusive<usize>`] would make more sense.
    /// This is the only place where we need to interface with [`codespan_reporting`]'s range, so
    /// rather than implementing `From<SourceRange> for Range<usize>` and confuse the rest of the codebase,
    /// we just do a double conversion here.
    fn to_range(source: SourceRange) -> std::ops::Range<usize> {
        let range = std::ops::RangeInclusive::<usize>::from(source);

        // Note that we are not adding 1 to the end when converting to a normal Range,
        // as this is what codespan_reporting expects for some reason.
        *range.start()..*range.end()
    }
}

/// Contains all of the contextual information for this specific instance of the diagnostic message.
///
/// A [`DiagnosticContext`] should be provided when creating a [`Diagnostic`] whenever possible,
/// as this will allow source code previews and additional notated messages within the rendered  message.
pub(crate) struct DiagnosticContext {
    message: DiagnosticMessage,
    labels: Vec<DiagnosticMessage>,
}

impl DiagnosticContext {
    pub(crate) fn new(message: DiagnosticMessage) -> Self {
        Self {
            message,
            labels: Vec::new(),
        }
    }

    pub(crate) fn with_labels(self, labels: Vec<DiagnosticMessage>) -> Self {
        Self {
            message: self.message,
            labels,
        }
    }
}

/// Different levels of diagnostic messages that a [`Diagnostic`] can produce.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum DiagnosticLevel {
    /// Error level messages need to be addressed for compilation to succeed.
    Error,
    /// Warning level messages should be addressed, but are not necessary for compilation to succeed.
    Warning,
}

/// Contains all the information necessary to render a compilation message.
pub(crate) struct Diagnostic {
    err: String,
    level: DiagnosticLevel,
    context: Option<DiagnosticContext>,
    suggestions: Vec<String>,
}

impl Diagnostic {
    pub(crate) fn new(err: &impl Error, level: DiagnosticLevel) -> Self {
        Self {
            err: err.to_string(),
            level,
            context: None,
            suggestions: Vec::new(),
        }
    }

    pub(crate) fn level(&self) -> DiagnosticLevel {
        self.level
    }

    pub(crate) fn with_context(self, context: DiagnosticContext) -> Self {
        Self {
            err: self.err,
            level: self.level,
            suggestions: self.suggestions,
            context: Some(context),
        }
    }

    pub(crate) fn with_suggestions(self, suggestions: Vec<impl Into<String>>) -> Self {
        Self {
            err: self.err,
            level: self.level,
            context: self.context,
            suggestions: suggestions
                .into_iter()
                .filter_map(|str| {
                    let str = str.into();
                    if str.len() > 0 {
                        Some(str)
                    } else {
                        None
                    }
                })
                .collect(),
        }
    }

    fn to_diagnostic<F: Copy>(self, file_id: F) -> codespan_reporting::diagnostic::Diagnostic<F> {
        let mut labels = Vec::new();
        if let Some(context) = self.context {
            labels.push(context.message.to_primary(file_id));
            labels.append(
                &mut context
                    .labels
                    .into_iter()
                    .map(|label| label.to_secondary(file_id))
                    .collect(),
            );
        }

        match self.level {
            DiagnosticLevel::Error => codespan_reporting::diagnostic::Diagnostic::error()
                .with_message(self.err)
                .with_labels(labels)
                .with_notes(self.suggestions),
            DiagnosticLevel::Warning => codespan_reporting::diagnostic::Diagnostic::warning()
                .with_message(self.err)
                .with_labels(labels)
                .with_notes(self.suggestions),
        }
    }
}

pub(crate) fn report_diagnostic(
    writer: &StandardStream,
    config: &codespan_reporting::term::Config,
    files: &SimpleFiles<&str, &str>,
    file_id: usize,
    message: Diagnostic,
) -> Result<(), RenderErrorFailure> {
    if let Err(err) = codespan_reporting::term::emit(
        &mut writer.lock(),
        config,
        files,
        &message.to_diagnostic(file_id),
    ) {
        return Err(RenderErrorFailure(err));
    }

    Ok(())
}

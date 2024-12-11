use std::collections::HashSet;

use thiserror::Error;

use crate::{
    diagnostic::{Diagnostic, DiagnosticContext, DiagnosticLevel, DiagnosticMessage},
    parser::r#struct::StructMember,
};

use super::{
    diagnostic::{diag_newly_defined, diag_originally_defined, diag_struct_name_label},
    Struct,
};

#[derive(Debug, Error)]
pub(super) enum StructError {
    #[error("duplicate struct member")]
    DuplicateMemberError {
        _struct: Struct,
        original: StructMember,
        new: StructMember,
    },
}

impl From<StructError> for Diagnostic {
    fn from(err: StructError) -> Self {
        match err {
            StructError::DuplicateMemberError {
                ref _struct,
                ref original,
                ref new,
            } => Self::new(&err, DiagnosticLevel::Error).with_context(
                DiagnosticContext::new(DiagnosticMessage::new(
                    format!(
                        "struct `{}` already has a member named `{}`",
                        _struct,
                        new.name()
                    ),
                    new.source(),
                ))
                .with_labels(vec![
                    diag_struct_name_label(_struct),
                    diag_originally_defined(original.source(), Some((*original.get_type()).into())),
                    diag_newly_defined(new.source(), Some((*new.get_type()).into())),
                ]),
            ),
        }
    }
}

pub(super) fn analyze_struct(_struct: &Struct) -> Vec<StructError> {
    let mut errors = Vec::new();

    // Check for any struct members with the same name
    let mut index = HashSet::new();
    for member in _struct.members() {
        if !index.insert(member) {
            errors.push(StructError::DuplicateMemberError {
                _struct: _struct.clone(),
                original: (*index.get(member).expect(
                    "couldn't find struct member after confirming it was already inserted",
                ))
                .clone(),
                new: member.clone(),
            });
        }
    }

    errors
}

use std::collections::HashSet;

use thiserror::Error;

use crate::{
    diagnostic::{Diagnostic, DiagnosticContext, DiagnosticLevel, DiagnosticMessage},
    parser::{r#struct::StructMember, types::DataType},
};

use super::{
    diagnostic::{diag_newly_defined, diag_originally_defined, diag_struct_name_label},
    scope_tracker::ScopeTracker,
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
    #[error("recursive struct definition")]
    RecursiveStructDefinitionError {
        _struct: Struct,
        recursive_member: StructMember,
    },
    #[error("unknown struct member type")]
    UnknownStructMemberTypeError {
        _struct: Struct,
        member: StructMember,
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
                    diag_originally_defined(original.source(), Some(original.get_type().into())),
                    diag_newly_defined(new.source(), Some(new.get_type().into())),
                ]),
            ),
            StructError::RecursiveStructDefinitionError {
                ref _struct,
                ref recursive_member,
            } => Self::new(&err, DiagnosticLevel::Error).with_context(
                DiagnosticContext::new(DiagnosticMessage::new(
                    format!(
                        "struct has infinite size due to member `{}`",
                        recursive_member.name()
                    ),
                    recursive_member.source(),
                ))
                .with_labels(vec![diag_struct_name_label(_struct)]),
            ),
            StructError::UnknownStructMemberTypeError {
                ref _struct,
                ref member,
            } => Self::new(&err, DiagnosticLevel::Error).with_context(
                DiagnosticContext::new(DiagnosticMessage::new(
                    format!(
                        "struct member `{}` has unknown type `{}`",
                        member.name(),
                        member.get_type()
                    ),
                    member.source(),
                ))
                .with_labels(vec![diag_struct_name_label(_struct)]),
            ),
        }
    }
}

pub(super) fn analyze_struct<'a>(
    _struct: &'a mut Struct,
    scope_tracker: &mut ScopeTracker,
) -> Vec<StructError> {
    let mut errors = Vec::new();

    // Check for any struct members with the same name
    let mut index = HashSet::new();
    for member in _struct.members() {
        if !index.insert(member) {
            errors.push(StructError::DuplicateMemberError {
                _struct: _struct.clone(),
                original: (*index
                    .get(member)
                    .expect("couldn't find struct member after it was inserted"))
                .clone(),
                new: member.clone(),
            });
        }
    }

    // This is done in a separate loop to satisfy the borrow checker.
    let struct_clone = _struct.clone();
    for member in _struct.members_mut() {
        match member.get_type_mut().into() {
            // Check if the member has the same type as the struct.
            // We cannot allow this because the struct would have an infinite size.
            &mut DataType::Struct { ref name, .. } if *name == struct_clone.name().to_string() => {
                errors.push(StructError::RecursiveStructDefinitionError {
                    _struct: struct_clone.clone(),
                    recursive_member: member.clone(),
                })
            }

            // Check if the member refers to a different struct but doesn't have a
            // copy of the struct populated yet. If not, populate it.
            &mut DataType::Struct {
                ref name,
                ref mut struct_data_types,
            } if struct_data_types.is_none() => {
                match scope_tracker.get_struct(&**name) {
                    Some(s) => {
                        let data_types: Vec<DataType> = s
                            .clone()
                            .into_members()
                            .into_iter()
                            .map(|member| member.into_type().into())
                            .collect();

                        *struct_data_types = Some(data_types.clone());

                        // Update the struct data types in the scope tracker as well.
                        scope_tracker.update_struct_data_types(
                            struct_clone.name(),
                            member.name(),
                            data_types,
                        );
                    }

                    // The struct this member is referring to doesn't exist.
                    None => errors.push(StructError::UnknownStructMemberTypeError {
                        _struct: struct_clone.clone(),
                        member: member.clone(),
                    }),
                }
            }

            // Don't do anything special for members with non-struct data types.
            _ => {}
        }
    }

    errors
}

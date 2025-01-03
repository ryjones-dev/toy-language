use std::collections::HashMap;

use thiserror::Error;

use crate::{
    diagnostic::{Diagnostic, DiagnosticContext, DiagnosticLevel, DiagnosticMessage},
    parser::{
        expression::{
            BinaryMathOperationType, BooleanComparisonType, Expression, UnaryMathOperationType,
        },
        function::FunctionSignature,
        identifier::Identifier,
        r#struct::StructMember,
        source_range::SourceRange,
        types::{DataType, Type, Types},
        variable::Variable,
    },
};

use super::{
    diagnostic::{
        diag_expected, diag_expected_types, diag_func_name_label, diag_func_param_label,
        diag_newly_defined, diag_originally_defined, diag_return_types_label,
        diag_struct_name_label,
    },
    scope::{analyze_scope, ScopeError},
    scope_tracker::ScopeTracker,
    Struct, EXPECT_STRUCT, EXPECT_VAR_TYPE,
};

#[derive(Debug, Error)]
pub(super) enum ExpressionError {
    #[error("wrong number of variables in assignment")]
    AssignmentWrongNumberOfVariablesError {
        expression: Expression,
        expected_len: usize,
        actual_len: usize,
        expected_source: SourceRange,
        actual_source: SourceRange,
    },
    #[error("assignment type mismatch")]
    AssignmentTypeMismatchError {
        expected_type: Type,
        actual_type: Type,
        assignment_source: SourceRange,
        expression: Expression,
    },
    #[error("variable type redefinition")]
    AssignmentVariableTypeRedefinitionError { prev_var: Variable, var: Variable },
    #[error("duplicate struct member in instantiation")]
    StructMemberAlreadyInitializedError {
        original_member: Identifier,
        new_member: Identifier,
    },
    #[error("struct member not initialized")]
    StructMemberNotInitializedError {
        _struct: Struct,
        member_name: Identifier,
        struct_instance_source: SourceRange,
    },
    #[error("unknown struct member")]
    StructMemberUnknownError {
        _struct: Struct,
        member_name: Identifier,
    },
    #[error("cannot access member on a non-struct type")]
    NonStructMemberAccessError {
        variable_name: Identifier,
        member: Identifier,
    },
    #[error("unknown struct")]
    StructUnknownError(String, SourceRange),
    #[error("return value mismatch")]
    FunctionReturnValueMismatchError {
        func_sig: FunctionSignature,
        return_types: Types,
        source_range: Option<SourceRange>,
    },
    #[error("unknown function")]
    FunctionCallUnknownError(Identifier, SourceRange),
    #[error("call to discarded function")]
    FunctionCallFunctionDiscardedError {
        name: Identifier,
        source_range: SourceRange,
        function_signature: FunctionSignature,
    },
    #[error("argument type mismatch")]
    FunctionCallArgumentTypeMismatchError {
        name: Identifier,
        argument_expression: Option<Expression>,
        source_range: SourceRange,
        function_signature: FunctionSignature,
        argument_types: Types,
    },
    #[error("mismatched return types for if expression")]
    IfElseReturnTypeMismatchError {
        source_range: SourceRange,
        then_types: Types,
        else_types: Types,
    },
    #[error("unknown variable")]
    VariableUnknownError(Variable),
    #[error("read from discarded variable")]
    VariableReadFromDiscardedError {
        variable: Variable,
        scope_var: Variable,
    },
    #[error("expression returns {value_count} values in a single value context")]
    ExpectedSingleValueError {
        value_count: usize,
        expression: Expression,
    },
    #[error("type mismatch")]
    TypeMismatchError {
        expected: DataType,
        actual: Type,
        expression: Expression,
    },
    #[error("unexpected type")]
    UnexpectedTypeError { ty: Type, source_range: SourceRange },
    #[error(transparent)]
    ScopeError(#[from] Box<ScopeError>),
}

impl From<ExpressionError> for Diagnostic {
    fn from(err: ExpressionError) -> Self {
        match err {
            ExpressionError::AssignmentWrongNumberOfVariablesError {
                ref expression,
                expected_len,
                actual_len,
                expected_source,
                actual_source,
            } => Self::new(&err, DiagnosticLevel::Error).with_context(
                DiagnosticContext::new(diag_expected(&expected_len, &actual_len, actual_source))
                    .with_labels({
                        let mut labels = vec![DiagnosticMessage::new(
                            format!(
                                "expression returns {} value{}",
                                expected_len,
                                if expected_len == 1 { "" } else { "s" }
                            ),
                            expected_source,
                        )];

                        if let Expression::FunctionCall {
                            function_signature, ..
                        } = expression
                        {
                            if let Some(func_sig) = function_signature {
                                if let Some(label) = diag_return_types_label(func_sig) {
                                    labels.push(label);
                                }
                            }
                        }

                        labels
                    }),
            ),
            ExpressionError::AssignmentTypeMismatchError {
                ref expected_type,
                ref actual_type,
                assignment_source,
                ref expression,
            } => Self::new(&err, DiagnosticLevel::Error).with_context(
                DiagnosticContext::new(DiagnosticMessage::new(
                    format!(
                        "attempted to assign result of type `{}` to variable of type `{}`",
                        expected_type, actual_type
                    ),
                    assignment_source,
                ))
                .with_labels({
                    let mut labels = vec![DiagnosticMessage::new(
                        "variable type defined here",
                        actual_type.source(),
                    )];
                    if let Expression::FunctionCall {
                        function_signature, ..
                    } = expression
                    {
                        if let Some(func_sig) = function_signature {
                            if let Some(label) = diag_return_types_label(func_sig) {
                                labels.push(label);
                            }
                        }
                    }
                    labels
                }),
            ),
            ExpressionError::AssignmentVariableTypeRedefinitionError {
                ref prev_var,
                ref var,
            } => Self::new(&err, DiagnosticLevel::Error).with_context(
                DiagnosticContext::new(diag_expected(
                    prev_var.get_type().as_ref().expect(EXPECT_VAR_TYPE),
                    var.get_type().as_ref().expect(EXPECT_VAR_TYPE),
                    var.source(),
                ))
                .with_labels(vec![
                    diag_originally_defined(
                        prev_var.source(),
                        prev_var.get_type().as_ref().map(|ty| ty.into()),
                    ),
                    diag_newly_defined(var.source(), var.get_type().as_ref().map(|ty| ty.into())),
                ]),
            ),
            ExpressionError::StructMemberAlreadyInitializedError {
                ref original_member,
                ref new_member,
            } => Self::new(&err, DiagnosticLevel::Error).with_context(
                DiagnosticContext::new(DiagnosticMessage::new(
                    format!(
                        "struct member `{}` has already been initialized",
                        new_member
                    ),
                    new_member.source(),
                ))
                .with_labels(vec![
                    diag_originally_defined(original_member.source(), None),
                    diag_newly_defined(new_member.source(), None),
                ]),
            ),
            ExpressionError::StructMemberNotInitializedError {
                ref _struct,
                ref member_name,
                struct_instance_source,
            } => Self::new(&err, DiagnosticLevel::Error).with_context(
                DiagnosticContext::new(DiagnosticMessage::new(
                    format!("struct member `{}` has not been initialized", member_name),
                    struct_instance_source,
                ))
                .with_labels(vec![diag_struct_name_label(_struct)]),
            ),
            ExpressionError::StructMemberUnknownError {
                ref _struct,
                ref member_name,
            } => Self::new(&err, DiagnosticLevel::Error).with_context(
                DiagnosticContext::new(DiagnosticMessage::new(
                    format!(
                        "Member `{}` is not defined on struct `{}`",
                        member_name,
                        _struct.name()
                    ),
                    member_name.source(),
                ))
                .with_labels(vec![diag_struct_name_label(_struct)]),
            ),
            ExpressionError::NonStructMemberAccessError {
                ref variable_name,
                ref member,
            } => Self::new(&err, DiagnosticLevel::Error).with_context(DiagnosticContext::new(
                DiagnosticMessage::new(
                    format!(
                        "`{}` is not a struct and cannot have member `{}`",
                        variable_name, member
                    ),
                    member.source(),
                ),
            )),
            ExpressionError::StructUnknownError(ref name, source) => {
                Self::new(&err, DiagnosticLevel::Error).with_context(DiagnosticContext::new(
                    DiagnosticMessage::new(
                        format!("unknown struct `{}` in this scope", name),
                        source,
                    ),
                ))
            }
            ExpressionError::FunctionReturnValueMismatchError {
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
            ExpressionError::FunctionCallUnknownError(ref name, source) => {
                Self::new(&err, DiagnosticLevel::Error).with_context(DiagnosticContext::new(
                    DiagnosticMessage::new(
                        format!("unknown function `{}` in this scope", name),
                        source,
                    ),
                ))
            }
            ExpressionError::FunctionCallFunctionDiscardedError {
                ref name,
                source_range,
                ref function_signature,
            } => Self::new(&err, DiagnosticLevel::Error)
                .with_context(
                    DiagnosticContext::new(DiagnosticMessage::new(
                        format!("call to discarded function `{}`", name),
                        source_range,
                    ))
                    .with_labels(vec![DiagnosticMessage::new(
                        format!("function `{}` defined here", function_signature.name),
                        function_signature.source,
                    )]),
                )
                .with_suggestions(vec![format!(
                    "Rename `{}` to `{}` to avoid discarding the function.",
                    name,
                    &name.to_string()[1..]
                )]),
            ExpressionError::FunctionCallArgumentTypeMismatchError {
                ref argument_expression,
                source_range,
                ref function_signature,
                ref argument_types,
                ..
            } => Self::new(&err, DiagnosticLevel::Error).with_context(
                DiagnosticContext::new(diag_expected(
                    &function_signature.params.types(),
                    argument_types,
                    if let Some(argument_expression) = argument_expression {
                        argument_expression.source()
                    } else {
                        source_range
                    },
                ))
                .with_labels({
                    let mut labels = vec![diag_func_name_label(&function_signature)];

                    if function_signature.params.len() > 0 {
                        labels.push(diag_func_param_label(&function_signature.params));
                    }

                    if let Some(expression) = argument_expression {
                        if let Expression::FunctionCall {
                            function_signature, ..
                        } = expression
                        {
                            if let Some(func_sig) = function_signature {
                                if let Some(label) = diag_return_types_label(func_sig) {
                                    labels.push(label);
                                }
                            }
                        }
                    }

                    labels
                }),
            ),
            ExpressionError::IfElseReturnTypeMismatchError {
                source_range,
                ref then_types,
                ref else_types,
            } => Self::new(&err, DiagnosticLevel::Error).with_context(
                DiagnosticContext::new(DiagnosticMessage::new(
                    "branches of if expression return different types",
                    source_range,
                ))
                .with_labels(vec![diag_expected_types(then_types, else_types)]),
            ),

            ExpressionError::VariableUnknownError(ref variable) => {
                Self::new(&err, DiagnosticLevel::Error).with_context(DiagnosticContext::new(
                    DiagnosticMessage::new(
                        format!("unknown variable `{}` in this scope", variable),
                        variable.source(),
                    ),
                ))
            }
            ExpressionError::VariableReadFromDiscardedError {
                ref variable,
                ref scope_var,
            } => Self::new(&err, DiagnosticLevel::Error)
                .with_context(
                    DiagnosticContext::new(DiagnosticMessage::new(
                        format!("read from discarded variable `{}`", variable),
                        variable.source(),
                    ))
                    .with_labels(vec![DiagnosticMessage::new(
                        format!("variable `{}` defined here", scope_var),
                        scope_var.source(),
                    )]),
                )
                .with_suggestions(vec![format!(
                    "Rename `{}` to `{}` to avoid discarding the variable.",
                    variable,
                    &variable.name().to_string()[1..]
                )]),
            ExpressionError::ExpectedSingleValueError {
                value_count,
                ref expression,
            } => Diagnostic::new(&err, DiagnosticLevel::Error).with_context(
                DiagnosticContext::new(DiagnosticMessage::new(
                    format!("expected single value, got {value_count}"),
                    expression.source(),
                ))
                .with_labels({
                    let mut labels = Vec::new();
                    if let Expression::FunctionCall {
                        function_signature, ..
                    } = expression
                    {
                        if let Some(func_sig) = function_signature {
                            if let Some(label) = diag_return_types_label(func_sig) {
                                labels.push(label);
                            }
                        }
                    }
                    labels
                }),
            ),
            ExpressionError::TypeMismatchError {
                ref expected,
                ref actual,
                ref expression,
            } => Self::new(&err, DiagnosticLevel::Error).with_context(
                DiagnosticContext::new(diag_expected(expected, actual.into(), expression.source()))
                    .with_labels({
                        let mut labels = Vec::new();
                        if let Expression::FunctionCall {
                            function_signature, ..
                        } = expression
                        {
                            if let Some(func_sig) = function_signature {
                                if let Some(label) = diag_return_types_label(func_sig) {
                                    labels.push(label);
                                }
                            }
                        }
                        labels
                    }),
            ),
            ExpressionError::UnexpectedTypeError {
                ref ty,
                source_range,
            } => Self::new(&err, DiagnosticLevel::Error).with_context(DiagnosticContext::new(
                DiagnosticMessage::new(format!("type {ty} cannot be used here"), source_range),
            )),
            ExpressionError::ScopeError(err) => (*err).into(),
        }
    }
}

/// Check if the given expression is semantically correct.
///
/// Recursively validate that inner expressions have the correct number and types of values
/// that the outer expression expects.
///
/// Returns the [`Types`] of values that the expression will return,
/// and a list of errors if any inner expression was not compatible with its outer expression.
pub(super) fn analyze_expression(
    expression: &mut Expression,
    scope_tracker: &mut ScopeTracker,
    outer_func_sig: &FunctionSignature,
) -> (Types, Vec<ExpressionError>) {
    match expression {
        Expression::Scope { scope, .. } => {
            // Create a new scope tracker to wrap the inner scope
            let inner_scope_tracker = ScopeTracker::new(Some(scope_tracker));

            let (types, errors) = analyze_scope(scope, inner_scope_tracker, outer_func_sig);
            let errors = errors
                .into_iter()
                .map(|err| ExpressionError::ScopeError(Box::new(err)))
                .collect();
            (types, errors)
        }
        Expression::ExpressionList { expressions, .. } => {
            let mut types = Types::new();
            let mut errors = Vec::new();

            for expression in expressions {
                let (mut tys, mut errs) =
                    analyze_expression(expression, scope_tracker, outer_func_sig);
                types.append(&mut tys);
                errors.append(&mut errs);
            }

            (types, errors)
        }
        Expression::Assignment { lhs, rhs, source } => {
            assert!(lhs.len() > 0, "there must always be at least one expression on the left hand side to form a valid assignment expression");

            let mut errors = Vec::new();

            // Analyze the right hand side expressions first to get the expected types
            let (rhs_types, mut errs) = analyze_expression(rhs, scope_tracker, outer_func_sig);
            errors.append(&mut errs);

            // If the number of lhs expressions does not equal the number of rhs types,
            // then we already know we have an error and cannot proceed with the rest of the analysis.
            if lhs.len() != rhs_types.len() {
                errors.push(ExpressionError::AssignmentWrongNumberOfVariablesError {
                    expression: *rhs.clone(),
                    expected_len: rhs_types.len(),
                    actual_len: lhs.len(),
                    expected_source: rhs_types.source().unwrap_or(rhs.source()),
                    actual_source: lhs
                        .first()
                        .unwrap()
                        .source()
                        .combine(lhs.last().unwrap().source()),
                });

                return (Types::new(), errors);
            }

            let mut lhs_types = Types::new();

            for (i, lhs_expression) in lhs.iter_mut().enumerate() {
                match lhs_expression {
                    // Handle variable expressions separately to not "read" the variable,
                    // which would prevent the variable from ever being unused.
                    Expression::Variable(variable) => {
                        if let None = variable.get_type() {
                            // Variable has not been annotated with a type,
                            // so give it the same type as the corresponding rhs expression's type.
                            variable.set_type(&rhs_types[i]);
                        }

                        // Add the variable to the scope
                        if let Some(scope_var) = scope_tracker.insert_var(variable.clone()) {
                            // The variable has already been added to the scope, so make sure it has the same type
                            if scope_var.get_type() != variable.get_type() {
                                errors.push(
                                    ExpressionError::AssignmentVariableTypeRedefinitionError {
                                        prev_var: scope_var.clone(),
                                        var: variable.clone(),
                                    },
                                );
                            }
                        }

                        lhs_types.push(variable.get_type().as_ref().unwrap().clone());
                    }

                    Expression::StructMemberAccess {
                        ..
                    } => {
                        let (mut types, mut errs) = analyze_expression(lhs_expression, scope_tracker, outer_func_sig);
                        lhs_types.append(&mut types);
                        errors.append(&mut errs);
                    }

                    _ => unreachable!("parsing guarantees that lhs expressions can only be variable or struct member access expressions")
                };
            }

            // Make sure that every corresponding type matches
            for (i, _) in lhs_types.iter().enumerate() {
                if lhs_types[i] != rhs_types[i] {
                    errors.push(ExpressionError::AssignmentTypeMismatchError {
                        expected_type: rhs_types[i].clone(),
                        actual_type: lhs_types[i].clone(),
                        assignment_source: *source,
                        expression: *rhs.clone(),
                    });
                }
            }

            // Assignment expressions should return no types
            (Types::new(), errors)
        }
        Expression::StructInstantiation {
            name,
            members,
            source,
            _struct,
        } => {
            let mut types = Types::new();
            let mut errors = Vec::new();

            let mut struct_data = HashMap::new();
            match scope_tracker.get_struct(&*name) {
                Some(existing_struct) => {
                    *_struct = Some(existing_struct.clone());

                    // Add the struct data to a hashmap for faster and easier lookups.
                    // This also releases the reference to the scope tracker so it can be borrowed
                    // again during member expression analysis.
                    for member in existing_struct.members() {
                        // This map tracks a tuple of the struct member and which instance member is used to initialize it.
                        struct_data.insert(
                            member.name().clone(),
                            (member.clone(), None::<&mut Identifier>),
                        );
                    }
                }
                None => {
                    errors.push(ExpressionError::StructUnknownError(
                        name.to_string(),
                        *source,
                    ));
                    return (types, errors);
                }
            }

            // Check if each member maps to the struct
            for (member_name, expression) in members {
                match struct_data.get_mut(member_name) {
                    Some((member, initialized_member_name)) => {
                        if let Some(initialized_member_name) = initialized_member_name {
                            errors.push(ExpressionError::StructMemberAlreadyInitializedError {
                                original_member: (*initialized_member_name).clone(),
                                new_member: member_name.clone(),
                            });
                        } else {
                            *initialized_member_name = Some(member_name);
                        }

                        let (tys, mut errs) =
                            analyze_expression(expression, scope_tracker, outer_func_sig);
                        errors.append(&mut errs);

                        if let Some(err) =
                            expect_single_type(expression, &tys, member.get_type().into())
                        {
                            errors.push(err);
                        }
                    }
                    None => errors.push(ExpressionError::StructMemberUnknownError {
                        _struct: _struct.as_ref().expect(EXPECT_STRUCT).clone(),
                        member_name: member_name.clone(),
                    }),
                }
            }

            // Check if there are any members that have not been initialized
            let uninitialized_members: Vec<StructMember> = struct_data
                .into_values()
                .filter_map(|(member, initialized_member_name)| {
                    if let None = initialized_member_name {
                        Some(member)
                    } else {
                        None
                    }
                })
                .collect();
            for uninitialized_member in uninitialized_members {
                errors.push(ExpressionError::StructMemberNotInitializedError {
                    _struct: _struct.as_ref().expect(EXPECT_STRUCT).clone(),
                    member_name: uninitialized_member.name().clone(),
                    struct_instance_source: *source,
                });
            }

            types.push(Type::new(
                DataType::Struct {
                    name: _struct.as_ref().expect(EXPECT_STRUCT).name().to_string(),
                    struct_data_types: _struct.clone().map(|s| {
                        s.into_members()
                            .into_iter()
                            .map(|member| member.into_type().into())
                            .collect()
                    }),
                },
                name.source(),
            ));

            (types, errors)
        }
        Expression::StructMemberAccess {
            variable,
            member_names,
            structs,
        } => {
            let mut types = Types::new();
            let mut errors = Vec::new();

            match scope_tracker.get_var(variable.name()) {
                Some(scope_var) => {
                    // Throw an error when trying to read from a discarded variable
                    if scope_var.is_discarded() {
                        errors.push(ExpressionError::VariableReadFromDiscardedError {
                            variable: variable.clone(),
                            scope_var: scope_var.clone(),
                        });
                    }

                    let var_type = scope_var.get_type().as_ref().expect(EXPECT_VAR_TYPE);

                    // We have to set the variable type on this AST node
                    // so that the type is available in code generation.
                    variable.set_type(var_type);

                    assert!(member_names.len() > 0, "should have at least one member name to be considered a struct member access");

                    // Check the type for each level of access to ensure the access is valid
                    let mut prev_member_name = variable.name();
                    let mut current_member_type = var_type;
                    let mut found_err = false;
                    for member_name in member_names.iter() {
                        match struct_data_and_member_type(
                            current_member_type,
                            member_name,
                            scope_tracker,
                            prev_member_name,
                        ) {
                            Ok(result) => {
                                prev_member_name = member_name;

                                let current_member_struct;
                                (current_member_struct, current_member_type) = result;

                                structs.push(current_member_struct.clone());
                            }
                            Err(err) => {
                                errors.push(err);
                                found_err = true;
                                break;
                            }
                        }
                    }

                    if !found_err {
                        types.push(current_member_type.clone());
                    }
                }
                None => {
                    errors.push(ExpressionError::VariableUnknownError(variable.clone()));
                }
            }

            (types, errors)
        }
        Expression::FunctionReturn { expression, .. } => {
            let (types, mut errors) = analyze_expression(expression, scope_tracker, outer_func_sig);

            if types != outer_func_sig.returns {
                errors.push(ExpressionError::FunctionReturnValueMismatchError {
                    func_sig: outer_func_sig.clone(),
                    return_types: types,
                    source_range: Some(expression.source()),
                })
            }

            // Function returns do not return any types here.
            // Keep in mind that we are returning the expression's resulting types,
            // which is not the same as the function's return types.
            (Types::new(), errors)
        }
        Expression::FunctionCall {
            name,
            argument_expression,
            source,
            function_signature,
        } => {
            let mut types = Types::new();

            let (func_sig, errors) = analyze_function_call(
                name,
                argument_expression,
                &source,
                scope_tracker,
                outer_func_sig,
            );
            match func_sig {
                Some(func_sig) => {
                    types = func_sig.returns.clone();

                    // Update the function call's cached signature in the AST
                    *function_signature = Some(func_sig);
                }
                None => {}
            }

            (types, errors)
        }
        Expression::IfElse {
            cond_expression,
            then_expression,
            else_expression,
            source,
        } => {
            let mut errors = Vec::new();

            // Analyze condition expression
            let (cond_types, mut errs) =
                analyze_expression(cond_expression, scope_tracker, outer_func_sig);
            errors.append(&mut errs);

            if let Some(err) = expect_single_type(cond_expression, &cond_types, &DataType::Bool) {
                errors.push(err);
            }

            // Analyze then expression
            let (then_types, mut errs) =
                analyze_expression(then_expression, scope_tracker, outer_func_sig);
            errors.append(&mut errs);

            let mut types_to_return = then_types;

            // Analyze else expression
            if let Some(else_expression) = else_expression.as_mut() {
                let (else_types, mut errs) =
                    analyze_expression(else_expression, scope_tracker, outer_func_sig);
                errors.append(&mut errs);

                // Check for mismatched branch types
                if types_to_return != else_types {
                    if let None = else_expression.last_function_return() {
                        if let Some(_) = then_expression.last_function_return() {
                            // Only consider the branches mismatched if neither of them
                            // end with a function return
                            types_to_return = else_types;
                        } else {
                            errors.push(ExpressionError::IfElseReturnTypeMismatchError {
                                source_range: *source,
                                then_types: types_to_return.clone(),
                                else_types,
                            })
                        }
                    }
                } else {
                    // Special case: If both branches of the if expression have function returns,
                    // extract the function return out of each branch.
                    // This is needed so that when an if expression is the last expression in a function,
                    // the special case for checking function returns will apply to the if expression.
                    // For more on this special case for function returns, see `semantic::function::analyze_function`.
                    if let Some(Expression::FunctionReturn {
                        expression: then_inner_expression,
                        ..
                    }) = then_expression.last_function_return()
                    {
                        if let Some(Expression::FunctionReturn {
                            expression: else_inner_expression,
                            ..
                        }) = else_expression.last_function_return()
                        {
                            // Convert function return to normal expression return on each branch
                            **then_expression = (**then_inner_expression).clone();
                            *else_expression = (**else_inner_expression).clone();

                            // Wrap entire if expression in function return
                            *expression = Expression::FunctionReturn {
                                expression: Box::new(Expression::IfElse {
                                    cond_expression: cond_expression.clone(),
                                    then_expression: then_expression.clone(),
                                    else_expression: Box::new(Some(else_expression.clone())),
                                    source: *source,
                                }),
                                source: *source,
                            };
                        }
                    }
                }
            }

            (types_to_return, errors)
        }
        Expression::BooleanComparison {
            comparison_type,
            lhs,
            rhs,
            ref source,
        } => {
            let mut errors = Vec::new();

            let (lhs_types, mut errs) = analyze_expression(lhs, scope_tracker, outer_func_sig);
            errors.append(&mut errs);
            let (rhs_types, mut errs) = analyze_expression(rhs, scope_tracker, outer_func_sig);
            errors.append(&mut errs);

            let mut types_to_return = Types::new();

            // Only continue if the lhs or rhs did not have errors
            if errors.len() == 0 {
                match comparison_type {
                    BooleanComparisonType::Equal | BooleanComparisonType::NotEqual => {
                        match expect_single_equal_types(lhs, rhs, &lhs_types, &rhs_types) {
                            Ok(ty) => match ty.into() {
                                &DataType::Struct {
                                    name: _,
                                    struct_data_types: _,
                                } => {
                                    unimplemented!(
                                        "comparing equality for structs is not yet implemented"
                                    )
                                }
                                _ => types_to_return.push(Type::new(DataType::Bool, *source)),
                            },
                            Err(err) => errors.push(err),
                        }
                    }
                    BooleanComparisonType::LessThan
                    | BooleanComparisonType::LessThanEqual
                    | BooleanComparisonType::GreaterThan
                    | BooleanComparisonType::GreaterThanEqual => {
                        match expect_single_equal_types(lhs, rhs, &lhs_types, &rhs_types) {
                            Ok(ty) => match ty.into() {
                                &DataType::Int => {
                                    types_to_return.push(Type::new(DataType::Bool, *source))
                                }
                                &DataType::Float => {
                                    types_to_return.push(Type::new(DataType::Bool, *source))
                                }
                                &DataType::Bool => {
                                    errors.push(ExpressionError::UnexpectedTypeError {
                                        ty: lhs_types[0].clone(),
                                        source_range: *source,
                                    })
                                }
                                &DataType::Struct {
                                    name: _,
                                    struct_data_types: _,
                                } => {
                                    unimplemented!(
                                        "comparing ordering for structs is not yet implemented"
                                    )
                                }
                            },
                            Err(err) => errors.push(err),
                        }
                    }
                }
            }

            (types_to_return, errors)
        }
        Expression::BinaryMathOperation {
            operation_type,
            lhs,
            rhs,
            ref source,
        } => {
            let mut errors = Vec::new();

            let (lhs_types, mut errs) = analyze_expression(lhs, scope_tracker, outer_func_sig);
            errors.append(&mut errs);
            let (rhs_types, mut errs) = analyze_expression(rhs, scope_tracker, outer_func_sig);
            errors.append(&mut errs);

            let mut types_to_return = Types::new();

            // Only continue if the lhs or rhs did not have errors
            if errors.len() == 0 {
                match operation_type {
                    BinaryMathOperationType::Add
                    | BinaryMathOperationType::Subtract
                    | BinaryMathOperationType::Multiply
                    | BinaryMathOperationType::Divide => {
                        match expect_single_equal_types(lhs, rhs, &lhs_types, &rhs_types) {
                            Ok(ty) => match ty.into() {
                                &DataType::Int => {
                                    types_to_return.push(Type::new(DataType::Int, *source))
                                }
                                &DataType::Float => {
                                    types_to_return.push(Type::new(DataType::Float, *source))
                                }
                                &DataType::Bool => {
                                    errors.push(ExpressionError::UnexpectedTypeError {
                                        ty: lhs_types[0].clone(),
                                        source_range: *source,
                                    })
                                }
                                &DataType::Struct {
                                    name: _,
                                    struct_data_types: _,
                                } => {
                                    unimplemented!("binary math for structs is not yet implemented")
                                }
                            },
                            Err(err) => errors.push(err),
                        }
                    }
                }
            }

            (types_to_return, errors)
        }
        Expression::UnaryMathOperation {
            operation_type,
            expression,
            ref source,
        } => {
            let mut errors = Vec::new();

            let (types, mut errs) = analyze_expression(expression, scope_tracker, outer_func_sig);
            errors.append(&mut errs);

            let mut types_to_return = Types::new();

            // Only continue if the inner expression did not have errors
            if errors.len() == 0 {
                match operation_type {
                    UnaryMathOperationType::Negate => {
                        match expect_any_single_type(expression, &types) {
                            Some(err) => errors.push(err),
                            None => match (&types[0]).into() {
                                &DataType::Int => {
                                    types_to_return.push(Type::new(DataType::Int, *source))
                                }
                                &DataType::Float => {
                                    types_to_return.push(Type::new(DataType::Float, *source))
                                }
                                &DataType::Bool => {
                                    errors.push(ExpressionError::UnexpectedTypeError {
                                        ty: types[0].clone(),
                                        source_range: *source,
                                    })
                                }
                                &DataType::Struct {
                                    name: _,
                                    struct_data_types: _,
                                } => {
                                    unimplemented!("unary math for structs is not yet implemented")
                                }
                            },
                        }
                    }
                }
            }

            (types_to_return, errors)
        }
        Expression::Variable(variable) => {
            let mut types = Types::new();
            let mut errors = Vec::new();

            match scope_tracker.get_var(variable.name()) {
                Some(scope_var) => {
                    // Throw an error when trying to read from a discarded variable
                    if scope_var.is_discarded() {
                        errors.push(ExpressionError::VariableReadFromDiscardedError {
                            variable: variable.clone(),
                            scope_var: scope_var.clone(),
                        });
                    }

                    // We have to set the variable type on this AST node
                    // so that the type is available in code generation.
                    variable.set_type(scope_var.get_type().as_ref().expect(EXPECT_VAR_TYPE));

                    types.push(variable.get_type().clone().unwrap());
                }
                None => {
                    errors.push(ExpressionError::VariableUnknownError(variable.clone()));
                }
            }

            (types, errors)
        }
        Expression::IntLiteral(literal) => {
            let mut types = Types::new();
            types.push(Type::new(DataType::Int, literal.source()));
            (types, Vec::new())
        }
        Expression::FloatLiteral(literal) => {
            let mut types = Types::new();
            types.push(Type::new(DataType::Float, literal.source()));
            (types, Vec::new())
        }
        Expression::BoolLiteral(literal) => {
            let mut types = Types::new();
            types.push(Type::new(DataType::Bool, literal.source()));
            (types, Vec::new())
        }
    }
}

pub(super) fn analyze_function_call(
    name: &Identifier,
    argument_expression: &mut Option<Expression>,
    source: &SourceRange,
    scope_tracker: &mut ScopeTracker,
    outer_func_sig: &FunctionSignature,
) -> (Option<FunctionSignature>, Vec<ExpressionError>) {
    let mut errors = Vec::new();

    // Analyze each argument expression to determine their types
    let mut argument_types = Types::new();

    if let Some(argument_expression) = argument_expression {
        let (types, mut errs) =
            analyze_expression(argument_expression, scope_tracker, outer_func_sig);
        argument_types = types;
        errors.append(&mut errs);
    }

    match scope_tracker.get_func_sig(name) {
        Some(func_sig) => {
            // Only add the type mismatch error if the argument expressions did not have errors,
            // otherwise this error would just add noise.
            if errors.len() == 0 {
                // Check that the function parameter types match the function call arguments' types
                if func_sig.params.types() != argument_types {
                    errors.push(ExpressionError::FunctionCallArgumentTypeMismatchError {
                        name: name.clone(),
                        argument_expression: argument_expression.clone(),
                        source_range: *source,
                        function_signature: func_sig.clone(),
                        argument_types,
                    });
                }
            }

            // Check if the function being called is discarded
            if func_sig.is_discarded() {
                errors.push(ExpressionError::FunctionCallFunctionDiscardedError {
                    name: name.clone(),
                    source_range: *source,
                    function_signature: func_sig.clone(),
                });
            }

            (Some(func_sig.clone()), errors)
        }
        None => {
            errors.push(ExpressionError::FunctionCallUnknownError(
                name.clone(),
                source.clone(),
            ));
            (None, errors)
        }
    }
}

/// Given a [`Type`] and member name, return the [`Struct`] data that the type refers to along with the [`Type`] of the member.
fn struct_data_and_member_type<'a>(
    ty: &Type,
    member_name: &Identifier,
    scope_tracker: &'a ScopeTracker,
    var_name: &Identifier,
) -> Result<(&'a Struct, &'a Type), ExpressionError> {
    match ty.into() {
        &DataType::Struct { ref name, .. } => match scope_tracker.get_struct(&**name) {
            Some(s) => s
                .members()
                .iter()
                .find_map(|member| {
                    if *member.name() == *member_name {
                        Some((s, member.get_type()))
                    } else {
                        None
                    }
                })
                .ok_or(ExpressionError::StructMemberUnknownError {
                    _struct: s.clone(),
                    member_name: member_name.clone(),
                }),
            None => Err(ExpressionError::StructUnknownError(
                name.clone(),
                var_name.source(),
            )),
        },
        _ => Err(ExpressionError::NonStructMemberAccessError {
            variable_name: var_name.clone(),
            member: member_name.clone(),
        }),
    }
}

fn expect_any_single_type(expression: &Expression, types: &Types) -> Option<ExpressionError> {
    if types.len() != 1 {
        return Some(ExpressionError::ExpectedSingleValueError {
            expression: expression.clone(),
            value_count: types.len(),
        });
    }

    None
}

fn expect_single_type(
    expression: &Expression,
    types: &Types,
    expected_type: &DataType,
) -> Option<ExpressionError> {
    if let Some(err) = expect_any_single_type(expression, types) {
        return Some(err);
    }

    if types[0] != *expected_type {
        return Some(ExpressionError::TypeMismatchError {
            expected: expected_type.clone(),
            actual: types[0].clone(),
            expression: expression.clone(),
        });
    }

    None
}

fn expect_single_equal_types<'a>(
    lhs_expression: &Expression,
    rhs_expression: &Expression,
    lhs_types: &'a Types,
    rhs_types: &Types,
) -> Result<&'a Type, ExpressionError> {
    if let Some(err) = expect_any_single_type(lhs_expression, lhs_types) {
        return Err(err);
    }

    if let Some(err) = expect_single_type(rhs_expression, rhs_types, (&lhs_types[0]).into()) {
        return Err(err);
    }

    Ok(&lhs_types[0])
}

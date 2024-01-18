use thiserror::Error;

use crate::{
    diagnostic::{Diagnostic, DiagnosticContext, DiagnosticLevel, DiagnosticMessage},
    parser::{
        expression::{
            BinaryMathOperationType, BooleanComparisonType, Expression, UnaryMathOperationType,
        },
        function::FunctionSignature,
        identifier::Identifier,
        source_range::SourceRange,
        types::{DataType, Type, Types},
        variable::{Variable, Variables},
    },
};

use super::{
    diagnostic::{
        diag_expected, diag_expected_types, diag_func_name_label, diag_func_param_label,
        diag_newly_defined, diag_originally_defined, diag_return_types_label,
    },
    scope::{analyze_scope, ScopeError},
    scope_tracker::ScopeTracker,
    EXPECT_VAR_TYPE,
};

#[derive(Debug, Error)]
pub(super) enum ExpressionError {
    #[error("wrong number of variables in assignment")]
    AssignmentWrongNumberOfVariablesError {
        expression: Expression,
        expected: Types,
        actual: Variables,
    },
    #[error("assignment type mismatch")]
    AssignmentTypeMismatchError {
        expected_type: Type,
        var: Variable,
        assignment_source: SourceRange,
        expression: Expression,
    },
    #[error("variable type redefinition")]
    AssignmentVariableTypeRedefinitionError { prev_var: Variable, var: Variable },
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
    #[error(transparent)]
    ScopeError(#[from] Box<ScopeError>),
}

impl From<ExpressionError> for Diagnostic {
    fn from(err: ExpressionError) -> Self {
        match err {
            ExpressionError::AssignmentWrongNumberOfVariablesError {
                ref expression,
                ref expected,
                ref actual,
            } => Self::new(&err, DiagnosticLevel::Error).with_context(
                DiagnosticContext::new(diag_expected(
                    &expected.len(),
                    &actual.len(),
                    actual.source().unwrap(), // There must be at least 1 variable for this error to occur
                ))
                .with_labels({
                    let mut labels = vec![DiagnosticMessage::new(
                        format!(
                            "expression returns {} value{}",
                            expected.len(),
                            if expected.len() == 1 { "" } else { "s" }
                        ),
                        expression.source(),
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
                ref var,
                assignment_source,
                ref expression,
            } => Self::new(&err, DiagnosticLevel::Error).with_context(
                DiagnosticContext::new(DiagnosticMessage::new(
                    format!(
                        "attempted to assign result of type `{}` to variable of type `{}`",
                        expected_type,
                        var.get_type().expect(EXPECT_VAR_TYPE)
                    ),
                    assignment_source,
                ))
                .with_labels({
                    let mut labels = vec![DiagnosticMessage::new(
                        "variable type defined here",
                        var.get_type().as_ref().expect(EXPECT_VAR_TYPE).source(),
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
                        prev_var.get_type().map(|ty| ty.into()),
                    ),
                    diag_newly_defined(var.source(), var.get_type().map(|ty| ty.into())),
                ]),
            ),
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

                    for expression in argument_expression {
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
                expected,
                actual,
                ref expression,
            } => Self::new(&err, DiagnosticLevel::Error).with_context(
                DiagnosticContext::new(diag_expected(
                    &expected,
                    &actual.into(),
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
        Expression::Assignment {
            variables,
            expression,
            source,
        } => {
            let (types, mut errors) = analyze_expression(expression, scope_tracker, outer_func_sig);

            if types.len() != variables.len() {
                errors.push(ExpressionError::AssignmentWrongNumberOfVariablesError {
                    expression: *expression.clone(),
                    expected: types,
                    actual: variables.clone(),
                });
            } else {
                // Only continue if the expression did not have errors,
                // otherwise adding type errors will add noise
                if errors.len() == 0 {
                    for (i, variable) in variables.iter_mut().enumerate() {
                        if let Some(var_type) = variable.get_type() {
                            // Check if the expression's corresponding type matches the variable's annotated type
                            if *var_type != types[i] {
                                errors.push(ExpressionError::AssignmentTypeMismatchError {
                                    expected_type: types[i],
                                    var: variable.clone(),
                                    assignment_source: *source,
                                    expression: *expression.clone(),
                                });
                            }
                        } else {
                            // Variable has not been explicitly assigned a type,
                            // so give it the same type as the expression's corresponding type
                            variable.set_type(&types[i]);
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
                    }
                }
            }

            // Assignment expressions should return no types
            (Types::new(), errors)
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

            expect_single_type(cond_expression, &cond_types, DataType::Bool, &mut errors);

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
                        if expect_any_single_type(lhs, &lhs_types, &mut errors) {
                            if expect_single_type(rhs, &rhs_types, lhs_types[0], &mut errors) {
                                types_to_return.push(Type::new(DataType::Bool, *source));
                            }
                        }
                    }
                    BooleanComparisonType::LessThan
                    | BooleanComparisonType::LessThanEqual
                    | BooleanComparisonType::GreaterThan
                    | BooleanComparisonType::GreaterThanEqual => {
                        // TODO: Handle floats later
                        if expect_single_type(lhs, &lhs_types, DataType::Int, &mut errors) {
                            if expect_single_type(rhs, &rhs_types, DataType::Int, &mut errors) {
                                types_to_return.push(Type::new(DataType::Bool, *source));
                            }
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
                        // TODO: Handle floats later
                        if expect_single_type(lhs, &lhs_types, DataType::Int, &mut errors) {
                            if expect_single_type(rhs, &rhs_types, DataType::Int, &mut errors) {
                                types_to_return.push(Type::new(DataType::Int, *source));
                            }
                        }
                    }
                }
            }

            (types_to_return, errors)
        }
        Expression::UnaryMathOperation {
            operation_type,
            expression,
            source,
        } => {
            let mut errors = Vec::new();

            let (types, mut errs) = analyze_expression(expression, scope_tracker, outer_func_sig);
            errors.append(&mut errs);

            let mut types_to_return = Types::new();

            // Only continue if the inner expression did not have errors
            if errors.len() == 0 {
                match operation_type {
                    UnaryMathOperationType::Negate => {
                        // TODO: Handle floats later
                        if expect_single_type(expression, &types, DataType::Int, &mut errors) {
                            types_to_return.push(Type::new(DataType::Int, *source))
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
                    } else {
                        // Because parsing a variable expression doesn't say anything about the variable's type,
                        // the Variable won't have its type set. Since the variable has already been added to the scope,
                        // we can update the variable's type here so as to not leave any undefined types in the AST.
                        variable.set_type(&scope_var.get_type().expect(EXPECT_VAR_TYPE));
                        types.push(variable.get_type().unwrap());
                    }
                }
                None => {
                    errors.push(ExpressionError::VariableUnknownError(variable.clone()));
                }
            }

            (types, errors)
        }
        Expression::IntLiteral(_, source) => {
            let mut types = Types::new();
            types.push(Type::new(DataType::Int, *source));
            (types, Vec::new())
        }
        Expression::BoolLiteral(_, source) => {
            let mut types = Types::new();
            types.push(Type::new(DataType::Bool, *source));
            (types, Vec::new())
        }
    }
}

pub(super) fn analyze_function_call(
    name: &Identifier,
    argument_expression: &mut Option<Expression>,
    source: &SourceRange,
    scope_tracker: &mut ScopeTracker,
    func_sig: &FunctionSignature,
) -> (Option<FunctionSignature>, Vec<ExpressionError>) {
    let mut errors = Vec::new();

    // Analyze each argument expression to determine their types
    let mut argument_types = Types::new();

    if let Some(argument_expression) = argument_expression {
        let (types, mut errs) = analyze_expression(argument_expression, scope_tracker, func_sig);
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

fn expect_any_single_type(
    expression: &Expression,
    types: &Types,
    errors: &mut Vec<ExpressionError>,
) -> bool {
    if types.len() != 1 {
        errors.push(ExpressionError::ExpectedSingleValueError {
            expression: expression.clone(),
            value_count: types.len(),
        });
        return false;
    }

    true
}

fn expect_single_type(
    expression: &Expression,
    types: &Types,
    expected_type: impl Into<DataType>,
    errors: &mut Vec<ExpressionError>,
) -> bool {
    if !expect_any_single_type(expression, types, errors) {
        return false;
    }

    let expected_type = expected_type.into();
    if types[0] != expected_type {
        errors.push(ExpressionError::TypeMismatchError {
            expected: expected_type,
            actual: types[0],
            expression: expression.clone(),
        });
        return false;
    }

    true
}

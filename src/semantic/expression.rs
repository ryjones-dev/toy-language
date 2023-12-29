use thiserror::Error;

use crate::{
    diagnostic::{Diagnostic, DiagnosticContext, DiagnosticLevel, DiagnosticMessage},
    parser::{
        expression::{
            BinaryMathOperationType, BooleanComparisonType, Expression, UnaryMathOperationType,
        },
        function::{FunctionCall, FunctionSignature},
        types::{DataType, Type, Types},
        variable::Variable,
    },
};

use super::{
    diagnostic::{
        diag_expected, diag_func_name_label, diag_func_param_label, diag_return_types_label,
    },
    scope::Scope,
    EXPECT_TYPES, EXPECT_VAR_TYPE,
};

#[derive(Debug, Error)]
pub(super) enum ExpressionError {
    #[error("unknown variable")]
    UnknownVariableError(Variable),
    #[error("unknown function")]
    UnknownFunctionError(FunctionCall),
    #[error("argument type mismatch")]
    ArgumentTypeMismatchError {
        func_sig: FunctionSignature,
        function_call: FunctionCall,
    },
    #[error("expected single value, but expression returns {0}")]
    SingleValueError(usize),
    #[error("expected type {expected}, but got {actual}")]
    WrongTypeError { expected: DataType, actual: Type },
}

impl From<ExpressionError> for Diagnostic {
    fn from(err: ExpressionError) -> Self {
        match err {
            ExpressionError::UnknownVariableError(ref variable) => {
                Self::new(&err, DiagnosticLevel::Error).with_context(DiagnosticContext::new(
                    DiagnosticMessage::new(
                        format!("unknown variable `{}` in this scope", variable),
                        variable.source(),
                    ),
                ))
            }
            ExpressionError::UnknownFunctionError(ref function_call) => {
                Self::new(&err, DiagnosticLevel::Error).with_context(DiagnosticContext::new(
                    DiagnosticMessage::new(
                        format!("unknown function `{}` in this scope", function_call.name),
                        function_call.source,
                    ),
                ))
            }
            ExpressionError::ArgumentTypeMismatchError {
                ref func_sig,
                ref function_call,
            } => Self::new(&err, DiagnosticLevel::Error).with_context(
                DiagnosticContext::new(diag_expected(
                    &func_sig.params.types(),
                    &function_call.argument_types.as_ref().expect(EXPECT_TYPES),
                    if function_call.arguments.len() > 0 {
                        function_call
                            .arguments
                            .first()
                            .unwrap()
                            .source()
                            .combine(function_call.arguments.last().unwrap().source())
                    } else {
                        function_call.source
                    },
                ))
                .with_labels({
                    let mut labels = vec![diag_func_name_label(func_sig)];

                    if func_sig.params.len() > 0 {
                        labels.push(diag_func_param_label(&func_sig.params));
                    }

                    for expression in &function_call.arguments {
                        if let Expression::FunctionCall(function_call) = expression {
                            labels.push(diag_return_types_label(
                                function_call.return_types.as_ref().expect(EXPECT_TYPES),
                            ));
                        }
                    }

                    labels
                }),
            ),
            ExpressionError::SingleValueError(_) => todo!(),
            ExpressionError::WrongTypeError { expected, actual } => todo!(),
        }
    }
}

/// Check if the given expression is semantically correct.
///
/// Recursively validate that inner expressions have the correct number and types of values that the outer expression expects.
/// Returns the expression's value types, and a list of errors if any inner expression was not compatible with its outer expression.
/// It's important that the types are always returned whenever possible so that variables are given the correct types in the calling code,
/// even if the expression is not valid.
pub(super) fn analyze_expression(
    expression: &mut Expression,
    scope: &Scope,
) -> (Types, Vec<ExpressionError>) {
    let mut types = Types::new();
    let mut errors = Vec::new();

    match expression {
        Expression::BooleanComparison {
            comparison_type,
            lhs,
            rhs,
            source,
        } => {
            let (lhs_types, mut errs) = analyze_expression(lhs, scope);
            errors.append(&mut errs);
            let (rhs_types, mut errs) = analyze_expression(rhs, scope);
            errors.append(&mut errs);

            match comparison_type {
                BooleanComparisonType::Equal | BooleanComparisonType::NotEqual => {
                    if expect_any_single_type(&lhs_types, &mut errors) {
                        if expect_single_type(&rhs_types, lhs_types[0], &mut errors) {
                            types.push(Type::new(DataType::Bool, *source));
                        }
                    }
                }
                // TODO: Handle floats later
                BooleanComparisonType::LessThan
                | BooleanComparisonType::LessThanEqual
                | BooleanComparisonType::GreaterThan
                | BooleanComparisonType::GreaterThanEqual => {
                    if expect_single_type(&lhs_types, DataType::Int, &mut errors) {
                        if expect_single_type(&rhs_types, DataType::Int, &mut errors) {
                            types.push(Type::new(DataType::Bool, *source));
                        }
                    }
                }
            };
        }
        Expression::BinaryMathOperation {
            operation_type,
            lhs,
            rhs,
            source,
        } => {
            match operation_type {
                BinaryMathOperationType::Add
                | BinaryMathOperationType::Subtract
                | BinaryMathOperationType::Multiply
                | BinaryMathOperationType::Divide => {
                    // TODO: Handle floats later
                    let (lhs_types, mut errs) = analyze_expression(lhs, scope);
                    errors.append(&mut errs);
                    let (rhs_types, mut errs) = analyze_expression(rhs, scope);
                    errors.append(&mut errs);

                    if expect_single_type(&lhs_types, DataType::Int, &mut errors) {
                        if expect_single_type(&rhs_types, DataType::Int, &mut errors) {
                            types.push(Type::new(DataType::Int, *source));
                        }
                    }
                }
            }
        }
        Expression::UnaryMathOperation {
            operation_type,
            expression,
            source,
        } => match operation_type {
            UnaryMathOperationType::Negate => {
                let (ty, mut errs) = analyze_expression(expression, scope);
                errors.append(&mut errs);

                if expect_single_type(&ty, DataType::Int, &mut errors) {
                    types.push(Type::new(DataType::Int, *source))
                }
            }
        },
        Expression::FunctionCall(function_call) => {
            let (func_sig, mut errs) = analyze_function_call(function_call, scope);
            match func_sig {
                Some(func_sig) => {
                    for return_type in &func_sig.returns {
                        types.push(*return_type);
                    }
                    errors.append(&mut errs);
                }
                None => errors.append(&mut errs),
            }
        }
        Expression::Variable(variable) => match scope.get_var(variable.name()) {
            Some(scope_var) => {
                // Because parsing a variable expression doesn't say anything about the variable's type,
                // the Variable won't have its type set. Since the variable has already been added to the scope,
                // we can update the variable's type here so as to not leave any undefined types in the AST.
                variable.set_type(&scope_var.get_type().expect(EXPECT_VAR_TYPE));
                types.push(variable.get_type().unwrap());
            }
            None => errors.push(ExpressionError::UnknownVariableError(variable.clone())),
        },
        Expression::IntLiteral(_, source) => types.push(Type::new(DataType::Int, *source)),
        Expression::BoolLiteral(_, source) => types.push(Type::new(DataType::Bool, *source)),
    };

    (types, errors)
}

pub(super) fn analyze_function_call<'a>(
    function_call: &mut FunctionCall,
    scope: &'a Scope,
) -> (Option<&'a FunctionSignature>, Vec<ExpressionError>) {
    let mut errors = Vec::new();

    match scope.get_func_sig(&function_call.name) {
        Some(func_sig) => {
            // Analyze each argument expression to determine their types
            let mut argument_types = Types::new();
            for expression in &mut function_call.arguments {
                let (mut args, mut errs) = analyze_expression(expression, scope);
                argument_types.append(&mut args);
                errors.append(&mut errs);
            }

            // Store the function's argument types and return types so codegen has access to them
            function_call.argument_types = Some(argument_types);
            function_call.return_types = Some(func_sig.returns.clone());

            // Check that the function parameters match the function call arguments
            if func_sig.params.types() != *function_call.argument_types.as_ref().unwrap() {
                errors.push(ExpressionError::ArgumentTypeMismatchError {
                    func_sig: func_sig.clone(),
                    function_call: function_call.clone(),
                });
            }

            (Some(func_sig), errors)
        }
        None => {
            errors.push(ExpressionError::UnknownFunctionError(function_call.clone()));
            (None, errors)
        }
    }
}

fn expect_any_single_type(types: &Types, errors: &mut Vec<ExpressionError>) -> bool {
    if types.len() != 1 {
        errors.push(ExpressionError::SingleValueError(types.len()));
        return false;
    }

    true
}

fn expect_single_type(
    types: &Types,
    expected_type: impl Into<DataType>,
    errors: &mut Vec<ExpressionError>,
) -> bool {
    if !expect_any_single_type(types, errors) {
        return false;
    }

    let expected_type = expected_type.into();
    if types[0] != expected_type {
        errors.push(ExpressionError::WrongTypeError {
            expected: expected_type,
            actual: types[0],
        });
        return false;
    }

    true
}

use thiserror::Error;

use crate::{
    diagnostic::Diagnostic,
    parser::{
        expression::{
            BinaryMathOperationType, BooleanComparisonType, Expression, UnaryMathOperationType,
        },
        function::{FunctionCall, FunctionSignature},
        identifier::Identifier,
        types::{DataType, Type, Types},
    },
};

use super::{scope::Scope, EXPECT_VAR_TYPE};

#[derive(Debug, Error)]
pub(super) enum ExpressionError {
    #[error("unknown variable \"{0}\" in this scope")]
    UnknownVariableError(Identifier),
    #[error("unknown function \"{0}\" in this scope")]
    UnknownFunctionError(Identifier),
    #[error("wrong number of arguments in call to function \"{function_name}\". expected: {expected}, actual: {actual}")]
    WrongNumberOfArgumentsError {
        function_name: Identifier,
        expected: usize,
        actual: usize,
    },
    #[error("mismatched argument type in call to function \"{function_name}\". expected: {expected}, actual: {actual}")]
    MismatchedArgumentTypeError {
        function_name: Identifier,
        expected: Type,
        actual: Type,
    },
    #[error("expected single value, but expression returns {0}")]
    SingleValueError(usize),
    #[error("expected type {expected}, but got {actual}")]
    WrongTypeError { expected: DataType, actual: Type },
}

impl From<ExpressionError> for Diagnostic {
    fn from(err: ExpressionError) -> Self {
        match err {
            ExpressionError::UnknownVariableError(_) => todo!(),
            ExpressionError::UnknownFunctionError(_) => todo!(),
            ExpressionError::WrongNumberOfArgumentsError {
                function_name,
                expected,
                actual,
            } => todo!(),
            ExpressionError::MismatchedArgumentTypeError {
                function_name,
                expected,
                actual,
            } => todo!(),
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
                let ty = scope_var.get_type().expect(EXPECT_VAR_TYPE);
                variable.set_type(&ty);
                types.push(ty);
            }
            None => errors.push(ExpressionError::UnknownVariableError(
                variable.name().clone(),
            )),
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

            // Check that the function parameters match the function call arguments
            if func_sig.params.len() != argument_types.len() {
                errors.push(ExpressionError::WrongNumberOfArgumentsError {
                    function_name: func_sig.name.clone(),
                    expected: func_sig.params.len(),
                    actual: argument_types.len(),
                });
            } else {
                for (i, arg) in argument_types.iter().enumerate() {
                    // Type check arguments
                    let param_type = func_sig.params[i].ty;
                    if *arg != param_type {
                        errors.push(ExpressionError::MismatchedArgumentTypeError {
                            function_name: func_sig.name.clone(),
                            expected: param_type,
                            actual: *arg,
                        });
                    }
                }
            }

            // Store the function's argument types and return types so codegen has access to them
            function_call.argument_types =
                Some(func_sig.params.iter().map(|param| param.ty).collect());
            function_call.return_types = Some(func_sig.returns.clone());

            (Some(func_sig), errors)
        }
        None => {
            errors.push(ExpressionError::UnknownFunctionError(
                function_call.name.clone(),
            ));
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

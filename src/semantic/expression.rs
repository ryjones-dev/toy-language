use thiserror::Error;

use crate::parser::{
    expression::{BooleanComparisonType, Expression},
    function::FunctionCall,
    identifier::Identifier,
    types::{Type, Types},
};

use super::{scope::Scope, EXPECT_VAR_TYPE};

#[derive(Debug, Error)]
pub enum ExpressionError {
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
    WrongTypeError { expected: Type, actual: Type },
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
        Expression::BooleanComparison(comparison_type, lhs, rhs) => {
            let (lhs_types, mut errs) = analyze_expression(lhs, scope);
            errors.append(&mut errs);

            let (rhs_types, mut errs) = analyze_expression(rhs, scope);
            errors.append(&mut errs);

            match comparison_type {
                BooleanComparisonType::Equal | BooleanComparisonType::NotEqual => {
                    if expect_any_single_type(&lhs_types, &mut errors) {
                        expect_single_type(&rhs_types, lhs_types[0], &mut errors);
                    }
                }
                // TODO: Handle floats later
                BooleanComparisonType::LessThan
                | BooleanComparisonType::LessThanEqual
                | BooleanComparisonType::GreaterThan
                | BooleanComparisonType::GreaterThanEqual => {
                    if expect_single_type(&lhs_types, Type::Int, &mut errors) {
                        expect_single_type(&rhs_types, Type::Int, &mut errors);
                    }
                }
            };

            types.push(Type::Bool);
        }
        Expression::BinaryMathOperation(operation_type, lhs, rhs) => {
            // TODO: Handle floats later
            let (lhs_types, mut errs) = analyze_expression(lhs, scope);
            errors.append(&mut errs);
            expect_single_type(&lhs_types, Type::Int, &mut errors);

            let (rhs_types, mut errs) = analyze_expression(rhs, scope);
            errors.append(&mut errs);
            expect_single_type(&rhs_types, Type::Int, &mut errors);

            types.push(Type::Int);
        }
        Expression::UnaryMathOperation(operation_type, expression) => {
            let (ty, mut errs) = analyze_expression(expression, scope);
            errors.append(&mut errs);
            expect_single_type(&ty, Type::Int, &mut errors);

            types.push(Type::Int)
        }
        Expression::FunctionCall(function_call) => {
            let (mut tys, mut errs) = analyze_function_call(function_call, scope);
            types.append(&mut tys);
            errors.append(&mut errs);
        }
        Expression::Variable(variable) => match scope.get_var(&variable.name) {
            Some(scope_var) => {
                // Because parsing a variable expression doesn't say anything about the variable's type,
                // the Variable won't have its type set. Since the variable has already been added to the scope,
                // we can update the variable's type here so as to not leave any undefined types in the AST.
                variable.ty = scope_var.ty;
                types.push(variable.ty.expect(EXPECT_VAR_TYPE))
            }
            None => errors.push(ExpressionError::UnknownVariableError(variable.name.clone())),
        },
        Expression::IntLiteral(_) => types.push(Type::Int),
        Expression::BoolLiteral(_) => types.push(Type::Bool),
    };

    (types, errors)
}

pub(super) fn analyze_function_call(
    function_call: &mut FunctionCall,
    scope: &Scope,
) -> (Types, Vec<ExpressionError>) {
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
                    let param_type = func_sig.params[i].ty.expect(EXPECT_VAR_TYPE);
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
            function_call.argument_types = Some(
                func_sig
                    .params
                    .iter()
                    .map(|param| param.ty.expect(EXPECT_VAR_TYPE))
                    .collect(),
            );
            function_call.return_types = Some(func_sig.returns.clone());

            // The function's return types are what should be propagated up to the call site
            (func_sig.returns.clone(), errors)
        }
        None => {
            errors.push(ExpressionError::UnknownFunctionError(
                function_call.name.clone(),
            ));
            (Types::new(), errors)
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
    expected_type: Type,
    errors: &mut Vec<ExpressionError>,
) -> bool {
    if !expect_any_single_type(types, errors) {
        return false;
    }

    if types[0] != expected_type {
        errors.push(ExpressionError::WrongTypeError {
            expected: expected_type,
            actual: types[0],
        });
        return false;
    }

    true
}

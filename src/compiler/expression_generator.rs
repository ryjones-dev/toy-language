use crate::{
    ast_parser::types::{
        BinaryMathOperationType, BooleanComparisonType, Expression, FunctionCall, Identifier,
        UnaryMathOperationType,
    },
    semantic::scope::Scope,
};
use cranelift::prelude::*;

use crate::semantic_assert;

/// A distinct type that is used to represent name the value of an evaluated [`Expression`].
///
/// In the current implementation, this is just a wrapper around Cranelift's [`Value`]
/// and is completely convertable to and from [`Value`].
/// It is intended to be used in places where it semantically makes sense to represent
/// an expression resulting value after evaluation.
/// [`Value`] also conveys that meaning, but it is a library-specific type that shouldn't be exposed to users.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub(super) struct ExpressionValue(Value);

impl From<ExpressionValue> for Value {
    fn from(value: ExpressionValue) -> Self {
        value.0
    }
}

impl From<Value> for ExpressionValue {
    fn from(value: Value) -> Self {
        Self { 0: value }
    }
}

/// Helper type that is responsible for generating Cranelift IR from expressions.
pub(super) struct ExpressionGenerator<
    'module,
    'ctx: 'builder,
    'builder,
    'scope,
    M: cranelift_module::Module + 'module,
> {
    builder: &'builder mut FunctionBuilder<'ctx>,
    module: &'module mut M,
    scope: &'scope Scope<'scope>,
}

impl<'module, 'ctx: 'builder, 'builder, 'scope, M: cranelift_module::Module>
    ExpressionGenerator<'module, 'ctx, 'builder, 'scope, M>
{
    pub(super) fn new(
        module: &'module mut M,
        builder: &'builder mut FunctionBuilder<'ctx>,
        scope: &'scope Scope<'scope>,
    ) -> Self {
        Self {
            builder,
            module,
            scope,
        }
    }
}

impl<'module, 'ctx: 'builder, 'builder, 'var, M: cranelift_module::Module + 'module>
    ExpressionGenerator<'module, 'ctx, 'builder, 'var, M>
{
    /// Generate IR for a given expression and return the expression's resulting values.
    ///
    /// An expression can return multiple values depending on the context.
    /// This function assumes that the context has already been checked by the [`crate::semantic`] module,
    /// so no error needs to be returned.
    pub(super) fn generate(&mut self, expression: Expression) -> Vec<ExpressionValue> {
        match expression {
            Expression::BooleanComparison(comparison_type, lhs, rhs) => {
                vec![self.generate_boolean_comparison(comparison_type, *lhs, *rhs)]
            }
            Expression::BinaryMathOperation(operation_type, lhs, rhs) => {
                vec![self.generate_binary_operation(operation_type, *lhs, *rhs)]
            }
            Expression::UnaryMathOperation(operation_type, expression) => {
                vec![self.generate_unary_operation(operation_type, *expression)]
            }
            Expression::FunctionCall(function_call) => self.generate_function_call(function_call),
            Expression::Variable(name) => vec![self.generate_variable(name)],
            Expression::IntLiteral(value) => vec![self.generate_int_literal(value)],
        }
    }

    fn generate_boolean_comparison(
        &mut self,
        comparison_type: BooleanComparisonType,
        lhs: Expression,
        rhs: Expression,
    ) -> ExpressionValue {
        let left_values = self.generate(lhs);
        let right_values = self.generate(rhs);
        semantic_assert!(
            left_values.len() == 1,
            "left boolean operand expression did not return a single value"
        );
        semantic_assert!(
            right_values.len() == 1,
            "right boolean operand expression did not return a single value"
        );
        let left_value = left_values[0];
        let right_value = right_values[0];
        match comparison_type {
            BooleanComparisonType::Equal => ExpressionValue(self.builder.ins().icmp(
                IntCC::Equal,
                Value::from(left_value),
                Value::from(right_value),
            )),
            BooleanComparisonType::NotEqual => ExpressionValue(self.builder.ins().icmp(
                IntCC::NotEqual,
                Value::from(left_value),
                Value::from(right_value),
            )),
            BooleanComparisonType::LessThan => ExpressionValue(self.builder.ins().icmp(
                IntCC::SignedLessThan,
                Value::from(left_value),
                Value::from(right_value),
            )),
            BooleanComparisonType::LessThanEqual => ExpressionValue(self.builder.ins().icmp(
                IntCC::SignedLessThanOrEqual,
                Value::from(left_value),
                Value::from(right_value),
            )),
            BooleanComparisonType::GreaterThan => ExpressionValue(self.builder.ins().icmp(
                IntCC::SignedGreaterThan,
                Value::from(left_value),
                Value::from(right_value),
            )),
            BooleanComparisonType::GreaterThanEqual => ExpressionValue(self.builder.ins().icmp(
                IntCC::SignedGreaterThanOrEqual,
                Value::from(left_value),
                Value::from(right_value),
            )),
        }
    }

    fn generate_binary_operation(
        &mut self,
        operation_type: BinaryMathOperationType,
        lhs: Expression,
        rhs: Expression,
    ) -> ExpressionValue {
        let left_values = self.generate(lhs);
        let right_values = self.generate(rhs);
        semantic_assert!(
            left_values.len() == 1,
            "left binary operand expression did not return a single value"
        );
        semantic_assert!(
            right_values.len() == 1,
            "right binary operand expression did not return a single value"
        );
        let left_value = left_values[0];
        let right_value = right_values[0];
        match operation_type {
            BinaryMathOperationType::Add => ExpressionValue(
                self.builder
                    .ins()
                    .iadd(Value::from(left_value), Value::from(right_value)),
            ),
            BinaryMathOperationType::Subtract => ExpressionValue(
                self.builder
                    .ins()
                    .isub(Value::from(left_value), Value::from(right_value)),
            ),
            BinaryMathOperationType::Multiply => ExpressionValue(
                self.builder
                    .ins()
                    .imul(Value::from(left_value), Value::from(right_value)),
            ),
            BinaryMathOperationType::Divide => ExpressionValue(
                self.builder
                    .ins()
                    .udiv(Value::from(left_value), Value::from(right_value)),
            ),
        }
    }

    fn generate_unary_operation(
        &mut self,
        operation_type: UnaryMathOperationType,
        expression: Expression,
    ) -> ExpressionValue {
        let values = self.generate(expression);
        semantic_assert!(
            values.len() == 1,
            "unary operand expression did not return a single value"
        );
        match operation_type {
            UnaryMathOperationType::Negate => {
                ExpressionValue(self.builder.ins().ineg(Value::from(values[0])))
            }
        }
    }

    fn generate_function_call(
        &mut self,
        FunctionCall { name, arguments }: FunctionCall,
    ) -> Vec<ExpressionValue> {
        // TODO: only support 64-bit integer types for now
        let int_type = codegen::ir::Type::int(64).unwrap();

        // Because we've already done semantic analysis, we know that the function being called is defined.
        // Therefore, we can directly declare the function in Cranelift while calling it, to allow for
        // any arbitrary function definition order.
        let func_sig = self.scope.get_func_sig(&name);
        semantic_assert!(
            func_sig.is_some(),
            format!("function \"{}\" not in scope", name)
        );
        let func_sig = func_sig.unwrap();

        let mut sig = self.module.make_signature();
        for _ in &func_sig.params {
            sig.params.push(AbiParam::new(int_type))
        }

        for _ in &func_sig.returns {
            sig.returns.push(AbiParam::new(int_type))
        }

        let func_id_to_call = self
            .module
            .declare_function(&String::from(name), cranelift_module::Linkage::Local, &sig)
            .unwrap(); // TODO: this could have a genuine error but it's extremely unlikely, so it's not worth changing the API right now

        // Get the function to be called
        let func_ref_to_call = self
            .module
            .declare_func_in_func(func_id_to_call, self.builder.func);

        // Generate IR for parameter expressions
        let mut arg_values = Vec::with_capacity(arguments.len());
        for arg in arguments {
            let values = self.generate(arg);
            semantic_assert!(
                values.len() == 1,
                "function argument expression did not return a single value"
            );
            arg_values.push(Value::from(values[0]));
        }

        // Call function
        let call = self.builder.ins().call(func_ref_to_call, &arg_values);

        // Return function return values
        self.builder
            .inst_results(call)
            .iter()
            .map(|value| ExpressionValue::from(*value))
            .collect::<Vec<ExpressionValue>>()
    }

    fn generate_variable(&mut self, name: Identifier) -> ExpressionValue {
        let variable = self.scope.get_var(&name);
        semantic_assert!(
            variable.is_some(),
            format!("variable \"{}\" not found in scope", name)
        );

        let variable = variable.unwrap();
        ExpressionValue::from(
            self.builder
                .use_var(cranelift::frontend::Variable::from(*variable)),
        )
    }

    fn generate_int_literal(
        &mut self,
        value: crate::ast_parser::types::IntLiteral,
    ) -> ExpressionValue {
        // TODO: only support 64-bit integers for now
        let int_type = codegen::ir::Type::int(64).unwrap();
        ExpressionValue(self.builder.ins().iconst(int_type, i64::from(value)))
    }
}

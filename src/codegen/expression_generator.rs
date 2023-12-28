use cranelift::{
    codegen::ir::{condcodes::IntCC, AbiParam, InstBuilder, Value},
    frontend::FunctionBuilder,
};

use crate::{
    parser::{
        expression::{
            BinaryMathOperationType, BooleanComparisonType, Expression, UnaryMathOperationType,
        },
        function::FunctionCall,
        identifier::Identifier,
        literals::{BoolLiteral, IntLiteral},
        types::DataType,
    },
    semantic_assert,
};

use super::block::BlockVariables;

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
    'block,
    M: cranelift_module::Module + 'module,
> {
    builder: &'builder mut FunctionBuilder<'ctx>,
    module: &'module mut M,
    block_vars: &'block mut BlockVariables,
}

impl<'module, 'ctx: 'builder, 'builder, 'block, M: cranelift_module::Module>
    ExpressionGenerator<'module, 'ctx, 'builder, 'block, M>
{
    pub(super) fn new(
        module: &'module mut M,
        builder: &'builder mut FunctionBuilder<'ctx>,
        block_vars: &'block mut BlockVariables,
    ) -> Self {
        Self {
            builder,
            module,
            block_vars,
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
            Expression::BooleanComparison {
                comparison_type,
                lhs,
                rhs,
                ..
            } => {
                vec![self.generate_boolean_comparison(comparison_type, *lhs, *rhs)]
            }
            Expression::BinaryMathOperation {
                operation_type,
                lhs,
                rhs,
                ..
            } => {
                vec![self.generate_binary_operation(operation_type, *lhs, *rhs)]
            }
            Expression::UnaryMathOperation {
                operation_type,
                expression,
                ..
            } => {
                vec![self.generate_unary_operation(operation_type, *expression)]
            }
            Expression::FunctionCall(function_call) => self.generate_function_call(function_call),
            Expression::Variable(variable) => vec![self.generate_variable(variable.name)],
            Expression::IntLiteral(value) => vec![self.generate_int_literal(value)],
            Expression::BoolLiteral(value) => vec![self.generate_bool_literal(value)],
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
        FunctionCall {
            name,
            arguments,
            argument_types,
            return_types,
            ..
        }: FunctionCall,
    ) -> Vec<ExpressionValue> {
        // Because we've already done semantic analysis, we know that the function being called is defined,
        // and this call to that function is correct.
        // Therefore, we can build the function signature from the function call information.
        // This allows for any arbitrary function definition order.
        let mut sig = self.module.make_signature();

        if let Some(argument_types) = argument_types {
            for ty in &argument_types {
                sig.params.push(AbiParam::new(ty.into()))
            }
        }

        if let Some(return_types) = return_types {
            for ty in &return_types {
                sig.returns.push(AbiParam::new(ty.into()));
            }
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
        ExpressionValue::from(
            self.builder
                .use_var(cranelift::frontend::Variable::from_u32(
                    self.block_vars.var(name),
                )),
        )
    }

    fn generate_int_literal(&mut self, value: IntLiteral) -> ExpressionValue {
        ExpressionValue(
            self.builder
                .ins()
                .iconst::<i64>(DataType::Int.into(), value.into()),
        )
    }

    fn generate_bool_literal(&mut self, value: BoolLiteral) -> ExpressionValue {
        ExpressionValue(
            self.builder
                .ins()
                .iconst(DataType::Bool.into(), if value.into() { 1 } else { 0 }),
        )
    }
}

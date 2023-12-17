use cranelift::prelude::*;
use thiserror::Error;

use crate::{
    ast_parser::{
        grammar::parser,
        types::{Expression, Function, ParseError, Statement},
    },
    semantic::{
        scope::Scope,
        semantic::{semantic_analysis, SemanticError},
    },
    semantic_assert,
};

use super::{
    context::CompilerContext,
    expression_generator::ExpressionGenerator,
    options::{CompileOptions, OptimizationLevel},
};

/// An error that captures all errors that can be thrown during compilation.
#[derive(Debug, Error)]
pub enum CompileError {
    #[error("Error creating compiler: {0}")]
    CreateError(cranelift_module::ModuleError),
    #[error("Error parsing source code: {0}")]
    ParseError(ParseError),
    #[error("Semantic error: {}", .0.iter().fold(String::new(), |acc, err| format!("{acc}\n{}", err)))]
    SemanticErrors(Vec<SemanticError>),
    #[error("Error finalizing compilation: {0}")]
    FinalizeError(cranelift_module::ModuleError),
    #[error("Error declaring function {0}: {1}")]
    FunctionDeclarationError(String, String),
    #[error("Error defining function {0}: {1}")]
    FunctionDefinitionError(String, String),
}

/// Abstracts different types of compiler backends that can be used by [`Compiler`].
///
/// The main purpose for this abstraction is to support JIT and AOT compilation methods.
/// [`cranelift_module::Module`] takes care of most of this for us,
/// but some additional functions are needed to abstract specifics surrounding
/// emitting generated code, so that [`Compiler`] can work independently of the backend.
/// Currently, only [`cranelift_jit::JITModule`] is implemented.
pub trait CompilerModule: cranelift_module::Module {
    fn finalize(&mut self) -> Result<(), CompileError>;
}

impl CompilerModule for cranelift_jit::JITModule {
    fn finalize(&mut self) -> Result<(), CompileError> {
        self.finalize_definitions()
            .map_err(|err| CompileError::FinalizeError(err))?;

        Ok(())
    }
}

/// The user-facing type for compiling TODO_LANG_NAME source code.
///
/// This is the entry point of the API. After creating a [`Compiler`] with [`Compiler::new()`],
/// calling [`Compiler::compile()`] does all of the compilation.
/// Any [`CompilerModule`]-specific actions after that, to retrieve the compiled code or
/// create object files from it, are done through the underlying [`CompilerModule`].
pub struct Compiler<M: CompilerModule> {
    module: M,
    options: CompileOptions,
    main_function_id: Option<cranelift_module::FuncId>,
}

impl Compiler<cranelift_jit::JITModule> {
    /// Creates a new [`Compiler`] instance with a given [`OptimizationLevel`].
    pub fn new(options: CompileOptions) -> Result<Self, CompileError> {
        let builder = match options.optimization_level {
            OptimizationLevel::None => cranelift_jit::JITBuilder::with_flags(
                &[("opt_level", "none")],
                cranelift_module::default_libcall_names(),
            ),
            OptimizationLevel::Speed => cranelift_jit::JITBuilder::with_flags(
                &[("opt_level", "speed")],
                cranelift_module::default_libcall_names(),
            ),
            OptimizationLevel::SpeedAndSize => cranelift_jit::JITBuilder::with_flags(
                &[("opt_level", "speed_and_size")],
                cranelift_module::default_libcall_names(),
            ),
        }
        .map_err(|err| CompileError::CreateError(err))?;

        let module = cranelift_jit::JITModule::new(builder);

        Ok(Self {
            module,
            options,
            main_function_id: None,
        })
    }

    /// Returns a pointer to the main function after the source code is compiled,
    /// or an error if the source code does not include a main function.
    ///
    /// # Panics
    /// Panics if the source code has not been compiled yet.
    pub fn get_main_function(&self) -> Result<*const u8, CompileError> {
        match self.main_function_id {
            Some(main_function_id) => Ok(self.module.get_finalized_function(main_function_id)),
            None => Err(CompileError::SemanticErrors(vec![
                SemanticError::MissingMainError,
            ])),
        }
    }
}

impl<M: CompilerModule> Compiler<M> {
    /// Compiles the given source code to machine code.
    ///
    /// Accessing the resulting machine code will depend on the implementation of the underlying [`CompilerModule`].
    /// For instance, a JIT module would give access to the main function in memory, while an object module
    /// would provide the machine code in object files so that they can be linked by a linker.
    ///
    /// This function can return many different types of errors at different stages of compilation,
    /// and all of those error types are represented by a [`CompileError`].
    ///
    /// If the intermediate IR is requested, this function will return that IR as a [`String`]
    /// in the first position of the tuple.
    /// If a disassembly is requested, this function will return that disassembly as a [`String`]
    /// in the second position of the tuple.
    pub fn compile(
        &mut self,
        source_code: &str,
    ) -> Result<(Option<String>, Option<String>), CompileError> {
        let mut context = CompilerContext::new(&self.module, self.options.request_disassembly);

        let mut ir = String::new();
        let mut disassembly = String::new();
        // Parse the source code into AST nodes
        let ast = parser::code(source_code).map_err(|err| CompileError::ParseError(err.into()))?;

        // Check that the code is semantically correct before attempting to generate code
        semantic_analysis(&ast).map_err(|errs| CompileError::SemanticErrors(errs))?;

        // Insert each function into the global scope so that function calls can be made without requiring them to be defined in order
        let mut global_scope = Scope::new(None);
        for function in ast.iter() {
            global_scope
                .insert_func_sig(&function.signature)
                .map_err(|err| {
                    semantic_assert!(false, format!("{}", err));
                    CompileError::SemanticErrors(vec![SemanticError::ScopeError(err)])
                })?;
        }

        // Compile each function individually, and build up the IR and/or disassembly if requested
        let mut function_context = FunctionBuilderContext::new();
        for function in ast {
            let (function_ir, function_disasm) = self.compile_function(
                &mut context,
                &mut function_context,
                Some(&global_scope),
                function,
            )?;

            match function_ir {
                Some(function_ir) => ir.push_str(format!("{}\n", function_ir).as_str()),
                None => {}
            }

            match function_disasm {
                Some(function_disasm) => {
                    disassembly.push_str(format!("{}\n", function_disasm).as_str())
                }
                None => {}
            }
        }

        // Finalize the compilation in a module-independent way
        self.module.finalize()?;

        Ok((
            match ir.as_str() {
                "" => None,
                _ => Some(ir),
            },
            match disassembly.as_str() {
                "" => None,
                _ => Some(disassembly),
            },
        ))
    }

    fn compile_function(
        &mut self,
        compiler_context: &mut CompilerContext,
        function_context: &mut FunctionBuilderContext,
        outer_scope: Option<&Scope>,
        function: Function,
    ) -> Result<(Option<String>, Option<String>), CompileError> {
        // Pull out the wrapped context
        let context = compiler_context.get_inner_context_mut();

        // TODO: only support 64-bit integer types for now
        let int_type = codegen::ir::Type::int(64).unwrap();

        // Add the function's parameters to the context
        for _ in &function.signature.params {
            context.func.signature.params.push(AbiParam::new(int_type));
        }

        // Add the function's return types to the context
        for _ in &function.signature.returns {
            context.func.signature.returns.push(AbiParam::new(int_type));
        }

        // We can now declare the function to Cranelift from the context
        let function_id = match &function.signature.name {
            Some(function_name) => {
                let function_name = String::from(function_name.clone());
                self.module
                    .declare_function(
                        &function_name,
                        cranelift_module::Linkage::Local,
                        &context.func.signature,
                    )
                    .map_err(|err| {
                        Self::check_codegen_error(&err);
                        CompileError::FunctionDeclarationError(function_name, err.to_string())
                    })?
            }
            None => todo!("Declare anonymous function"),
        };

        // Instantiate the function builder and create the function entry block where IR code will be emitted.
        // Because this is the entry block of the function, we can seal it early as no other blocks can branch to it.
        let mut builder = FunctionBuilder::new(&mut context.func, function_context);
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        // To start compiling the function body, we first need to create a new scope and
        // add the parameter variables to the scope so that the function may access them.
        let mut scope = Scope::new(outer_scope);
        for (i, name) in function.signature.params.iter().enumerate() {
            let variable = cranelift::frontend::Variable::from(*(scope.insert_var(name)));
            builder.declare_var(variable, int_type);

            // Because we know the values of the parameters from the arguments of the calling function,
            // we can define those values right away.
            let value = builder.block_params(entry_block)[i];
            builder.def_var(variable, value)
        }

        // Copy the function name as a string, since compiling the function body consumes the function
        let function_name = function
            .signature
            .name
            .clone()
            .map(|name| String::from(name));

        // Now we can compile the function body
        self.compile_function_body(builder, scope, function);

        // Mark the function as defined to kick off Cranelift IR compilation
        self.module
            .define_function(function_id, context)
            .map_err(|err| {
                Self::check_codegen_error(&err);
                CompileError::FunctionDefinitionError(
                    match function_name.clone() {
                        Some(function_name) => function_name,
                        None => "".to_string(),
                    },
                    err.to_string(),
                )
            })?;

        // Take note if this is the main function
        if let Some(function_name) = function_name {
            if function_name == "main" {
                self.main_function_id = Some(function_id);
            }
        }

        // Assume IR is requested first to appease context lifetimes
        let mut ir = Some(format!("{}", context.func));
        let disassembly = context.compiled_code().unwrap().vcode.clone();

        if !self.options.request_ir {
            ir = None;
        }

        compiler_context.clear(&self.module);

        Ok((ir, disassembly))
    }

    fn compile_function_body(
        &mut self,
        mut builder: FunctionBuilder,
        mut scope: Scope,
        function: Function,
    ) {
        // Emit IR code for each statement in the function
        for statement in function.body {
            self.compile_statement(&mut builder, &mut scope, statement);
        }

        builder.finalize();
    }

    fn compile_statement(
        &mut self,
        builder: &mut FunctionBuilder,
        scope: &mut Scope,
        statement: Statement,
    ) {
        // TODO: only support 64-bit integer types for now
        let int_type = codegen::ir::Type::int(64).unwrap();

        match statement {
            Statement::Assignment(variable_names, expression) => {
                // Generate IR for the expression on the right-hand side of the equals sign
                let values;
                {
                    let mut expression_generator =
                        ExpressionGenerator::new(&mut self.module, builder, scope);
                    values = expression_generator.generate(expression);
                }

                // Declare and define the variables
                for (i, variable_name) in variable_names.into_iter().enumerate() {
                    // Only declare and define variable identifiers if they are not the discard identifier.
                    if let Some(variable_name) = variable_name {
                        let variable = *match scope.get_var(&variable_name) {
                            Some(variable) => variable,
                            None => {
                                let variable = scope.insert_var(&variable_name);

                                // Declare the variable in Cranelift if it was not in scope
                                builder.declare_var(variable.clone().into(), int_type);

                                variable
                            }
                        };

                        // Define the variable's value to the corresponding return value of the expression
                        builder.def_var(variable.into(), values[i].into());
                    }
                }
            }
            Statement::FunctionCall(function_call) => {
                let mut expression_generator =
                    ExpressionGenerator::new(&mut self.module, builder, scope);

                expression_generator.generate(Expression::FunctionCall(function_call));
            }
            Statement::Return(expressions) => {
                let mut return_values = Vec::new();

                for expression in expressions {
                    // Generate IR for the return value expressions
                    let mut values;
                    {
                        let mut expression_generator =
                            ExpressionGenerator::new(&mut self.module, builder, scope);
                        values = expression_generator.generate(expression);
                    }

                    return_values.append(&mut values);
                }

                // Add the IR instruction to actually return from the function
                builder.ins().return_(
                    &return_values
                        .iter()
                        .map(|value| Value::from(*value))
                        .collect::<Vec<Value>>(),
                );
            }
        }
    }

    /// Check if a given module error is caused by an error in the source code, or a bug in the compiler.
    ///
    /// Panics if the error is actually due to a bug in the compiler.
    /// This is helpful while iterating and debugging the compiler.
    fn check_codegen_error(err: &cranelift_module::ModuleError) {
        match err {
            cranelift_module::ModuleError::Compilation(ref inner_err) => match inner_err {
                codegen::CodegenError::Verifier(inner_err) => panic!(
                    "Codegen error. This is a bug in the compiler.\n{}:\n{}",
                    err.to_string(),
                    inner_err.to_string()
                ),
                _ => {}
            },
            _ => {}
        }
    }
}

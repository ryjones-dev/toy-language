use thiserror::Error;

use crate::{
    ast_parser::{
        grammar::parser,
        types::{AbstractSyntaxTree, ParseError},
    },
    codegen::{
        codegen::{CodeGenError, CodeGenerator},
        options::CodeGenOptions,
    },
    semantic::{scope::Scope, semantic_analysis, SemanticError},
    semantic_assert,
};

/// An error that captures all errors that can be thrown during compilation.
#[derive(Debug, Error)]
pub enum CompileError {
    #[error("parse error: {0}")]
    ParseError(ParseError),
    #[error("semantic error: {}", .0.iter().fold(String::new(), |acc, err| format!("{acc}\n{}", err)))]
    SemanticErrors(Vec<SemanticError>),
    #[error("codegen error: {0}")]
    CodeGenError(#[from] CodeGenError),
}

fn frontend(source_code: &str) -> Result<(AbstractSyntaxTree, Scope), CompileError> {
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

    Ok((ast, global_scope))
}

/// The primary function responsible for compiling TODO_LANG_NAME source code to machine code ahead of time (AOT).
///
/// This is the entry point of the API. After creating a [`CodeGenOptions`] with [`CodeGenOptions::new()`],
/// calling this function does all of the compilation.
pub fn compile(
    source_code: &str,
    options: CodeGenOptions,
) -> Result<(Option<String>, Option<String>), CompileError> {
    todo!("support AOT object compilation")
}

/// The primary function responsible for compiling TODO_LANG_NAME source code to machine code just in time (JIT).
///
/// This is the entry point of the API. After creating a [`CodeGenOptions`] with [`CodeGenOptions::new()`],
/// calling this function does all of the compilation.
pub fn compile_jit(
    source_code: &str,
    options: CodeGenOptions,
) -> Result<(*const u8, Option<String>, Option<String>), CompileError> {
    let (ast, global_scope) = frontend(source_code)?;

    let mut code_generator = CodeGenerator::<cranelift_jit::JITModule>::new(options)?;

    let (ir, disassembly) = code_generator
        .generate(global_scope, ast)
        .map_err(|err| CompileError::CodeGenError(err))?;

    let code = code_generator
        .get_main_function()
        .ok_or(CompileError::SemanticErrors(vec![
            SemanticError::MissingMainError,
        ]))?;

    Ok((code, ir, disassembly))
}

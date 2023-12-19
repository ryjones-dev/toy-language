use thiserror::Error;

use crate::{
    codegen::{
        codegen::{CodeGenError, CodeGenerator},
        options::CodeGenOptions,
    },
    parser::{
        grammar::parser,
        types::{AbstractSyntaxTree, ParseError},
    },
    semantic::{semantic_analysis, SemanticError},
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

fn frontend(source_code: &str) -> Result<AbstractSyntaxTree, CompileError> {
    // Parse the source code into AST nodes
    let mut ast = parser::parse(source_code).map_err(|err| CompileError::ParseError(err.into()))?;

    // Check that the code is semantically correct before attempting to generate code
    semantic_analysis(&mut ast).map_err(|errs| CompileError::SemanticErrors(errs))?;

    Ok(ast)
}

fn jit_backend(
    ast: AbstractSyntaxTree,
    options: CodeGenOptions,
) -> Result<(*const u8, Option<String>, Option<String>), CompileError> {
    let mut code_generator = CodeGenerator::<cranelift_jit::JITModule>::new(options)?;

    let (ir, disassembly) = code_generator
        .generate(ast)
        .map_err(|err| CompileError::CodeGenError(err))?;

    let code = code_generator
        .get_main_function()
        .ok_or(CompileError::SemanticErrors(vec![
            SemanticError::MissingMainError,
        ]))?;

    Ok((code, ir, disassembly))
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
    let (ast) = frontend(source_code)?;

    unreachable!("frontend works");

    jit_backend(ast, options)
}

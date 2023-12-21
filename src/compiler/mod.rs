use codespan_reporting::{
    diagnostic::Diagnostic,
    files::SimpleFile,
    term::termcolor::{ColorChoice, StandardStream},
};
use thiserror::Error;

use self::{
    backend::jit_backend,
    frontend::{frontend, FrontendResults},
    options::CompileOptions,
};

mod backend;
mod frontend;
pub mod options;

/// An error that is returned when the compiler is unable to render error messages to stderr.
#[derive(Debug, Error)]
#[error("failed to render error message: {0}")]
pub struct RenderErrorFailure(codespan_reporting::files::Error);

pub enum JitCompileResults {
    Success {
        code: *const u8,
        ast: Option<String>,
        ir: Option<String>,
        disassembly: Option<String>,
    },
    ParseError,
    SemanticError {
        ast: Option<String>,
    },
    BackendError {
        ast: Option<String>,
    },
}

/// The primary function responsible for compiling TODO_LANG_NAME source code to machine code ahead of time (AOT).
///
/// This is the entry point of the API. After creating a [`CompileOptions`] with [`CompileOptions::new()`],
/// calling this function does all of the compilation.
///
/// All of the compiler's output can be found under [`TODO`]. Its contents will vary depending on the [`CompileOptions`].
///
/// The compiler will emit any error messages to stderr automatically. If an error message cannot be rendered,
/// [`RenderErrorFailure`] is returned.
pub fn compile(source_code: &str, options: CompileOptions) -> Result<(), RenderErrorFailure> {
    todo!("support AOT object compilation")
}

/// The primary function responsible for compiling TODO_LANG_NAME source code to machine code just in time (JIT).
///
/// This is the entry point of the API. After creating a [`CompileOptions`] with [`CompileOptions::new()`],
/// calling this function does all of the compilation.
///
/// All of the compiler's output can be found under [`JitCompileResults`]. Its contents will vary depending on the [`CompileOptions`].
///
/// The compiler will emit any error messages to stderr automatically. If an error message cannot be rendered,
/// [`RenderErrorFailure`] is returned.
///
/// If compilation fails with [`SemanticError`]s, or a [`BackendError`],
/// [`JitCompileResults`] will still contain the AST string, if requested.
pub fn compile_jit(
    source_code: &str,
    options: CompileOptions,
) -> Result<JitCompileResults, RenderErrorFailure> {
    // Setup for error message rendering
    let file = SimpleFile::new("code", source_code);
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();

    match frontend(source_code, &options) {
        FrontendResults::Success { ast, ast_string } => {
            match jit_backend(ast, options.codegen_options) {
                Ok(results) => Ok(JitCompileResults::Success {
                    code: results.code,
                    ast: ast_string,
                    ir: results.ir,
                    disassembly: results.disassembly,
                }),
                Err(err) => {
                    report_error(&writer, &config, &file, err)?;
                    Ok(JitCompileResults::BackendError { ast: ast_string })
                }
            }
        }
        FrontendResults::ParseError(err) => {
            report_error(&writer, &config, &file, err)?;
            Ok(JitCompileResults::ParseError)
        }
        FrontendResults::SemanticErrors { errs, ast_string } => {
            for err in errs {
                report_error(&writer, &config, &file, err)?;
            }

            Ok(JitCompileResults::SemanticError { ast: ast_string })
        }
    }
}

fn report_error<E: std::fmt::Display>(
    writer: &StandardStream,
    config: &codespan_reporting::term::Config,
    file: &SimpleFile<&str, &str>,
    error: E,
) -> Result<(), RenderErrorFailure> {
    let diagnostic = Diagnostic::error().with_message(error.to_string());
    if let Err(err) = codespan_reporting::term::emit(&mut writer.lock(), config, file, &diagnostic)
    {
        return Err(RenderErrorFailure(err));
    }

    Ok(())
}

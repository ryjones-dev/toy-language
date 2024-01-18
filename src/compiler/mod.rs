use codespan_reporting::{
    files::SimpleFiles,
    term::termcolor::{ColorChoice, StandardStream},
};

use crate::diagnostic::{report_diagnostic, Diagnostic, DiagnosticLevel, RenderErrorFailure};

use self::{
    backend::jit_backend,
    frontend::{frontend, FrontendResults},
    options::CompileOptions,
};

mod backend;
mod frontend;
pub mod options;

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
    let mut files = SimpleFiles::new();
    let file_id = files.add("code", source_code);
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();

    match frontend(source_code) {
        FrontendResults::Parsed { ast, errs } => {
            let mut ast_string = None;
            if options.request_ast {
                ast_string = Some(ast.to_string());
            }

            let diagnostics: Vec<Diagnostic> = errs.into_iter().map(|err| err.into()).collect();
            let mut should_codegen = !diagnostics
                .iter()
                .any(|diag| diag.level() == DiagnosticLevel::Error);

            for diag in diagnostics {
                report_diagnostic(&writer, &config, &files, file_id, diag)?;
            }

            if options.codegen_options.disabled {
                should_codegen = false;
            }

            if should_codegen {
                match jit_backend(ast, &options.codegen_options) {
                    Ok(results) => Ok(JitCompileResults::Success {
                        code: results.code,
                        ast: ast_string,
                        ir: results.ir,
                        disassembly: results.disassembly,
                    }),
                    Err(err) => {
                        report_diagnostic(&writer, &config, &files, file_id, err.into())?;
                        Ok(JitCompileResults::BackendError { ast: ast_string })
                    }
                }
            } else {
                Ok(JitCompileResults::SemanticError { ast: ast_string })
            }
        }
        FrontendResults::ParseError(err) => {
            report_diagnostic(&writer, &config, &files, file_id, err.into())?;
            Ok(JitCompileResults::ParseError)
        }
    }
}

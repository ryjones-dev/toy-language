use std::{env, fs};

use codegen::options::CodeGenOptions;
use diagnostic::RenderErrorFailure;

use crate::compiler::{compile_jit, options::CompileOptions, JitCompileResults};

// TODO: remove these?
mod codegen;
mod compiler;
mod diagnostic;
mod parser;
mod semantic;

// This macro is unsafe because it relies on the caller to provide the correct input and output types.
// Using incorrect types at this point may corrupt the program's state.
macro_rules! execute_jit {
    ($code:expr, $out_type:ty, $(($args:expr, $in_types:ty)),*) => {
        unsafe {
            // Cast the code bytes to a function pointer. This operation is inheritly unsafe,
            // because you have to trust that the compiled code is safe to execute.
            let code_fn = std::mem::transmute::<_, fn($($in_types),*) -> $out_type>($code);
            let output = code_fn($($args),+);
            println!("{}", output);
        }
    };
}

fn main() -> Result<(), RenderErrorFailure> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        println!("missing source filepath argument");
        return Ok(());
    }

    let source_code = match fs::read_to_string(&args[1]) {
        Ok(source_code) => source_code,
        Err(err) => {
            println!("{}", err);
            return Ok(());
        }
    };

    let codegen_options = CodeGenOptions::new()
        .enable(true)
        .with_ir(false)
        .with_disassembly(false);
    let compile_options = CompileOptions::new()
        .with_ast(false)
        .with_codegen_options(codegen_options);

    match compile_jit(&source_code, compile_options)? {
        JitCompileResults::Success {
            code,
            ast,
            ir,
            disassembly,
        } => {
            if let Some(ast) = ast {
                println!("{}", ast);
            }

            if let Some(ir) = ir {
                println!("{}", ir);
            }

            if let Some(disassembly) = disassembly {
                println!("{}", disassembly);
            }

            let arg1: f64 = 1.1;
            let arg2: f64 = 2.2;
            execute_jit!(code, f64, (arg1, f64), (arg2, f64));

            Ok(())
        }
        JitCompileResults::ParseError => Ok(()),
        JitCompileResults::SemanticError { ast } => {
            if let Some(ast) = ast {
                println!("{}", ast);
            }

            Ok(())
        }
        JitCompileResults::BackendError { ast } => {
            if let Some(ast) = ast {
                println!("{}", ast);
            }

            Ok(())
        }
    }
}

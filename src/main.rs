use codegen::options::CodeGenOptions;
use compiler::RenderErrorFailure;

use crate::compiler::{compile_jit, options::CompileOptions, JitCompileResults};

// TODO: remove these?
mod codegen;
mod compiler;
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

// TODO: this might not be needed with codespan-reporting crate or similar
// /// Convert a peg position to a (line, col) tuple.
// ///
// /// This function is copied from an older version of peg:
// /// https://github.com/kevinmehall/rust-peg/blob/8098fab1515dc38ca25cdebda9b17655aa5554e1/src/translate.rs#L189
// fn pos_to_line_col(input: &str, pos: usize) -> (usize, usize) {
//     let mut remaining = pos;
//     let mut lineno: usize = 1;
//     for line in input.lines() {
//         let line_length = line.len() + 1;
//         if remaining < line_length {
//             return (lineno, remaining + 1);
//         }
//         remaining -= line_length;
//         lineno += 1;
//     }
//     return (lineno, remaining + 1);
// }

fn main() -> Result<(), RenderErrorFailure> {
    let source_code = include_str!("lang/test.txt");

    let codegen_options = CodeGenOptions::new().with_ir(false).with_disassembly(false);
    let compile_options = CompileOptions::new()
        .with_ast(false)
        .with_codegen_options(codegen_options);

    match compile_jit(source_code, compile_options)? {
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

            let arg1: i64 = 1;
            let arg2: i64 = 2;
            execute_jit!(code, i64, (arg1, i64), (arg2, i64));

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

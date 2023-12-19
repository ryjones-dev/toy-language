use codegen::options::CodeGenOptions;
use compiler::CompileError;
use parser::{grammar::parser as grammar_parser, types::ParseError};

use crate::compiler::compile_jit;

// TODO: remove these?
mod codegen;
mod compiler;
mod parser;
mod semantic;

// For debugging syntax errors
fn print_parsed_ast(source_code: &str) -> Result<(), ParseError> {
    let code = grammar_parser::parse(source_code)?;
    for c in code {
        println!("{:?}", c);
        println!();
    }

    Ok(())
}

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

fn main() -> Result<(), CompileError> {
    let source_code = include_str!("lang/test_new.txt");

    // TODO: move this to the compiler
    match print_parsed_ast(source_code) {
        Ok(()) => {}
        Err(err) => {
            println!("{}", err);
            return Err(CompileError::ParseError(err));
        }
    }

    let codegen_options = CodeGenOptions::new().with_ir(true).with_disassembly(false);

    let (code, ir, disassembly) = match compile_jit(source_code, codegen_options) {
        Ok((code, ir, disassembly)) => (code, ir, disassembly),
        Err(err) => {
            println!("{}", err);
            return Err(err);
        }
    };

    match ir {
        Some(ir) => println!("{}", ir),
        None => {}
    }
    match disassembly {
        Some(disassembly) => println!("{}", disassembly),
        None => {}
    }

    let arg1: i64 = 1;
    let arg2: i64 = 2;
    execute_jit!(code, i64, (arg1, i64), (arg2, i64));

    Ok(())
}

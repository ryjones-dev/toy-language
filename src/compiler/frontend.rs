use crate::{
    parser::{ast::AbstractSyntaxTree, parser, ParseError},
    semantic::{semantic_analysis, SemanticError},
};

use super::options::CompileOptions;

pub(super) enum FrontendResults {
    Parsed {
        ast: AbstractSyntaxTree,
        ast_string: Option<String>,
        errs: Vec<SemanticError>,
    },
    ParseError(ParseError),
}

pub(super) fn frontend(source_code: &str, options: &CompileOptions) -> FrontendResults {
    let mut ast = match parser::parse(source_code).map_err(|err| err.into()) {
        Ok(ast) => ast,
        Err(err) => return FrontendResults::ParseError(err),
    };

    let mut ast_string = None;
    if options.request_ast {
        ast_string = Some(ast.to_string());
    }

    let errs = semantic_analysis(&mut ast);
    FrontendResults::Parsed {
        ast,
        ast_string,
        errs,
    }
}

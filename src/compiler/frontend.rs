use thiserror::Error;

use crate::{
    parser::{ast::AbstractSyntaxTree, parser, ParseError},
    semantic::{semantic_analysis, SemanticError},
};

use super::options::CompileOptions;

#[derive(Debug, Error)]
pub(super) enum FrontendError {
    #[error("parse error: {0}")]
    ParseError(#[from] ParseError),
    #[error("semantic error: {0}")]
    SemanticError(#[from] SemanticError),
}

pub(super) enum FrontendResults {
    Success {
        ast: AbstractSyntaxTree,
        ast_string: Option<String>,
    },
    ParseError(ParseError),
    SemanticErrors {
        errs: Vec<SemanticError>,
        ast_string: Option<String>,
    },
}

pub(super) fn frontend(source_code: &str, options: &CompileOptions) -> FrontendResults {
    let mut ast = match parser::parse(source_code).map_err(|err| err.into()) {
        Ok(ast) => ast,
        Err(err) => return FrontendResults::ParseError(err),
    };

    let mut ast_string = None;
    if options.request_ast {
        ast_string = Some(format!("{}", ast));
    }

    if let Err(errs) = semantic_analysis(&mut ast) {
        return FrontendResults::SemanticErrors { errs, ast_string };
    }

    FrontendResults::Success { ast, ast_string }
}

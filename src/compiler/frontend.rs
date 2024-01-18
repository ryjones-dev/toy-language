use crate::{
    parser::{ast::AbstractSyntaxTree, parser, ParseError},
    semantic::{semantic_analysis, SemanticError},
};

pub(super) enum FrontendResults {
    Parsed {
        ast: AbstractSyntaxTree,
        errs: Vec<SemanticError>,
    },
    ParseError(ParseError),
}

pub(super) fn frontend(source_code: &str) -> FrontendResults {
    let mut ast = match parser::parse(source_code).map_err(|err| err.into()) {
        Ok(ast) => ast,
        Err(err) => return FrontendResults::ParseError(err),
    };

    let errs = semantic_analysis(&mut ast);
    FrontendResults::Parsed { ast, errs }
}

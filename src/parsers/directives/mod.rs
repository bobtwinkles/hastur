//! Parser and handlers for the various directives
use crate::ast::AstNode;
use crate::evaluated::BlockSpan;
use crate::parsers::ast::parse_ast;
use crate::Engine;
use crate::ParseErrorKind;
use nom::IResult;

/// The parsed action
#[derive(Clone, Debug)]
pub(crate) enum Action {
    /// We found an include directive that should error on non-extant files.
    /// Includes the AST of the file names
    Include(AstNode),
    /// We found an include directive that will not error on non-extant files.
    /// Includes the AST of the file names
    SoftInclude(AstNode),
    /// Export directive, including the AST node of what we should export
    Export(AstNode),
    /// Unexport directive, including the AST node of what we should export
    Unexport(AstNode),
    /// vpath directive
    VPath(AstNode),
    /// load directive
    Load(AstNode),
    /// load directive, non-erroring variant
    SoftLoad(AstNode),
}

impl<'a> crate::parsers::ParserState<'a> {
    pub(crate) fn handle_directive_action(
        &mut self,
        engine: &mut Engine,
        action: Action,
    ) -> Result<(), ParseErrorKind> {
        match action {
            e => unimplemented!("Unimplemented action {:?}", e),
        }
        // Ok(())
    }
}

pub(crate) fn parse_line<'a>(i: BlockSpan<'a>) -> IResult<BlockSpan<'a>, Action, ParseErrorKind> {
    pe_fix!(
        i,
        alt!(
            preceded!(pe_fix!(tag_no_case!("export")), parse_ast) => { Action::Export }     |
            preceded!(pe_fix!(tag_no_case!("unexport")), parse_ast) => { Action::Unexport } |
            preceded!(pe_fix!(tag_no_case!("vpath")), parse_ast) => { Action::VPath }       |
            preceded!(pe_fix!(tag_no_case!("include")), parse_ast) => { Action::Include }   |
            preceded!(pe_fix!(tag_no_case!("sinclude")), parse_ast) => { Action::SoftInclude }|
            preceded!(pe_fix!(tag_no_case!("-include")), parse_ast) => { Action::SoftInclude }|
            preceded!(pe_fix!(tag_no_case!("load")), parse_ast) => { Action::Load }         |
            preceded!(pe_fix!(tag_no_case!("-load")), parse_ast) => { Action::SoftLoad }
        )
    )
}

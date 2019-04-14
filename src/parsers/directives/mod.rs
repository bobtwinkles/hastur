//! Parser and handlers for the various directives
use crate::ast::AstNode;
use crate::evaluated::BlockSpan;
use crate::parsers::ast::parse_ast;
use crate::{Engine, NameCache, ParseErrorKind};
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

impl crate::parsers::ParserState {
    pub(crate) fn handle_directive_action(
        &mut self,
        names: &mut NameCache,
        engine: &mut Engine,
        action: Action,
    ) -> Result<(), ParseErrorKind> {
        match action {
            Action::Include(ast) => self.handle_include(ast, names, engine, false),
            Action::SoftInclude(ast) => self.handle_include(ast, names, engine, true),
            e => unimplemented!("Unimplemented action {:?}", e),
        }
    }

    fn handle_include(
        &mut self,
        ast: AstNode,
        names: &mut NameCache,
        engine: &mut Engine,
        soft: bool,
    ) -> Result<(), ParseErrorKind> {
        use crate::MakefileError;
        use std::fs::File;

        use crate::parsers::file_sequence::{parse_file_seq, FileSeqParseOptions};

        let contents = ast.eval(names, engine);

        let mut seq_parse_options = FileSeqParseOptions::new(engine.working_directory.clone());
        seq_parse_options.check_ar = false;

        let files = parse_file_seq(contents.span(), seq_parse_options);

        for file in files {
            let mut path = engine.working_directory.clone();
            path.push(&file);

            let f = match File::open(&path) {
                Ok(f) => f,
                Err(e) => {
                    if soft {
                        // TODO: Route this through a warnings system
                        warn!("Failed to include file {:?}", &file);
                        continue;
                    } else {
                        return Err(ParseErrorKind::IncludeFailure(e.kind(), file));
                    }
                }
            };
            let mut br = std::io::BufReader::new(f);

            engine
                .read_makefile(names, &mut br, &file)
                .map_err(|e| match e {
                    MakefileError::IOError(e) => ParseErrorKind::IncludeFailure(e.kind(), file),
                    MakefileError::ParseError(p) => p,
                })?;
        }

        Ok(())
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

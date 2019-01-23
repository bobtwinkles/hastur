//! Parses a variable expansion AST out of the provided block span
use crate::ast;
use crate::ast::AstNode;
use crate::evaluated::BlockSpan;
use crate::ParseErrorKind;
use nom::IResult;

/// Extract a semantic variable evaluation AST from the provided block span
pub(super) fn parse_ast<'a>(i: BlockSpan<'a>) -> IResult<BlockSpan<'a>, AstNode, ParseErrorKind> {
    use nom::Slice;
    /// Right now just slam everything into one big happy block and call it good
    let start_location = i.segments().next().unwrap().location().clone();
    Ok((
        i.slice(i.len()..),
        ast::concat(
            start_location,
            i.segments()
                .map(|x| ast::constant(x.location().clone(), x.into()))
                .collect(),
        ),
    ))
}

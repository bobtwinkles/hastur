//! Parses a variable expansion AST out of the provided block span
use crate::ast;
use crate::ast::AstNode;
use crate::evaluated::BlockSpan;
use crate::source_location::Location;
use crate::ParseErrorKind;
use nom::IResult;

#[cfg(test)]
mod test;

/// Extract a semantic variable evaluation AST from the provided block span
pub(super) fn parse_ast<'a>(
    mut i: BlockSpan<'a>,
) -> IResult<BlockSpan<'a>, AstNode, ParseErrorKind> {
    use nom::Slice;
    let start_location = match i.location() {
        Some(v) => v,
        None => return Ok((i, ast::empty())),
    };

    let mut master_concat_nodes = Vec::new();

    while i.len() > 0 {
        let (new_i, (until_dollar, dollar_location)) = fix_error!(
            i,
            ParseErrorKind,
            alt!(
                  pair!(take_until!("$"), tag!("$")) => {
                      |(start, dollar): (BlockSpan<'a>, BlockSpan<'a>)| (start, Some(dollar.location().unwrap()))
                  }
                | nom::rest => { |l| (l, None) })
        )?;

        for segment in until_dollar.segments() {
            master_concat_nodes.push(ast::constant(segment.location().clone(), segment.into()));
        }

        i = new_i;

        let dollar_location = match dollar_location {
            Some(v) => v,
            None => {
                // We reached the end without finding a dollar sign
                break;
            }
        };

        let (new_i, child) = parse_var_ref(i, dollar_location)?;
        i = new_i;
        master_concat_nodes.push(child);
    }

    Ok((
        i,
        ast::collapsing_concat(start_location, master_concat_nodes),
    ))
}

fn parse_var_ref<'a>(
    i: BlockSpan<'a>,
    dollar_location: Location,
) -> IResult<BlockSpan<'a>, AstNode, ParseErrorKind> {
    alt!(
        i,
        fix_error!(ParseErrorKind, eof!()) => {{
            let location = dollar_location.clone();
            |l| {
                // Synthesize a magic '$'
                ast::constant(location.clone(), crate::source_location::LocatedString::new(location.into(), "$".into()))
            }
        }} |
        fix_error!(ParseErrorKind, tag!("$")) => {{
            let location = dollar_location.clone();

            |l: BlockSpan| {
                // Simple case: two $$ in a row evaluate to just a $
                let segment = l.segments().next().unwrap();
                ast::constant(location, segment.into())
            }
        }} |
        // Try the advanced variable references
        preceded!(fix_error!(ParseErrorKind, char!('(')), apply!(advanced_var, dollar_location.clone(), '(')) => { |l| l } |
        preceded!(fix_error!(ParseErrorKind, char!('}')), apply!(advanced_var, dollar_location.clone(), '{')) => { |l| l } |
        // Finally, just grab a single character and call it good
        fix_error!(ParseErrorKind, take!(1)) => {{
            let location = dollar_location.clone();
            |l: BlockSpan|
            ast::variable_reference(location, ast::constant(l.location().unwrap(), l.segments().next().unwrap().into()))
        }}
    )
}

fn advanced_var<'a>(
    i: BlockSpan<'a>,
    dollar_location: Location,
    sep_char: char,
) -> IResult<BlockSpan<'a>, AstNode, ParseErrorKind> {
    unimplemented!()
}

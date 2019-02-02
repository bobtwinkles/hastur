//! Parses a variable expansion AST out of the provided block span
use crate::ast;
use crate::ast::AstNode;
use crate::evaluated::BlockSpan;
use crate::parsers::{fail_out, makefile_whitespace};
use crate::source_location::Location;
use crate::ParseErrorKind;
use nom::{Err, ErrorKind, IResult};

#[cfg(test)]
mod test;

/// Extract a semantic variable evaluation AST from the provided block span
pub(crate) fn parse_ast<'a>(
    mut i: BlockSpan<'a>,
) -> IResult<BlockSpan<'a>, AstNode, ParseErrorKind> {
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
            |_| {
                // Synthesize a magic '$'
                ast::constant(
                    location.clone(),
                    crate::source_location::LocatedString::new(location.into(), "$".into())
                )
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
        preceded!(fix_error!(ParseErrorKind, char!('(')),
                  apply!(advanced_var, dollar_location.clone(), '(', ')')) => { |l| l } |
        preceded!(fix_error!(ParseErrorKind, char!('}')),
                  apply!(advanced_var, dollar_location.clone(), '{', '}')) => { |l| l } |
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
    start_char: char,
    term_char: char,
) -> IResult<BlockSpan<'a>, AstNode, ParseErrorKind> {
    use nom::{InputIter, InputTake, Slice};
    let mut count = 1;
    let mut it = i.iter_indices();
    let mut split_idx = 0;

    // Iterate to find the balanced character
    while count > 0 {
        let (idx, c) = match it.next() {
            Some(v) => v,
            None => {
                return fail_out(i, ParseErrorKind::UnternimatedVariable);
            }
        };
        split_idx = idx;
        if c == start_char {
            count += 1;
        } else if c == term_char {
            count -= 1;
        };
    }

    let (i, name_node) = i.take_split(split_idx + 1);
    let name_node = name_node.slice(..name_node.len() - 1);

    match function_call(name_node) {
        Ok(v) => Ok(v),
        Err(Err::Failure(context)) => {
            if context.clone().into_error_kind()
                == nom::ErrorKind::Custom(ParseErrorKind::InternalFailure("not a function call"))
            {
                let parsed = parse_ast(name_node)?;

                // quick sanity check as long as we're testing
                #[cfg(test)]
                assert_complete!(parsed.0);

                Ok((i, ast::variable_reference(dollar_location, parsed.1)))
            } else {
                Err(Err::Failure(context))
            }
        }
        v => v,
    }
}

fn function_call<'a>(i: BlockSpan<'a>) -> IResult<BlockSpan<'a>, AstNode, ParseErrorKind> {
    fn no_such_function<'a>(i: BlockSpan<'a>) -> IResult<BlockSpan<'a>, AstNode, ParseErrorKind> {
        Err(Err::Failure(nom::Context::Code(
            i,
            ErrorKind::Custom(ParseErrorKind::InternalFailure("not a function call")),
        )))
    }

    macro_rules! func_entry {
        ($i: expr, $t:literal, $f:expr) => {
            pe_complete!(
                $i,
                do_parse!(
                    func_name: pe_fix!(tag!($t))
                        >> many1!(makefile_whitespace)
                        >> parsed: apply!($f, func_name.location().unwrap())
                        >> (parsed)
                )
            )
        };
    }

    alt!(
        i,
        func_entry!("strip", strip)
            | func_entry!("words", words)
            | func_entry!("word", word)
            | pe_complete!(no_such_function)
    )
}

fn function_argument<'a>(
    i: BlockSpan<'a>,
) -> IResult<BlockSpan<'a>, BlockSpan<'a>, ParseErrorKind> {
    use nom::{InputIter, InputTake};

    // Strip off any leading whitespace
    let (i, _) = many0!(i, makefile_whitespace)?;

    let mut it = i.iter_indices();
    let mut paren_count = 0;
    let mut curly_count = 0;
    while let Some((j, c)) = it.next() {
        match c {
            '(' => paren_count += 1,
            ')' => {
                if paren_count > 0 {
                    paren_count -= 1
                } else {
                    // Somehow we ended up with unbalanced parens, abort
                    return fail_out(i, ParseErrorKind::UnternimatedVariable);
                }
            }
            '{' => curly_count += 1,
            '}' => {
                if curly_count > 0 {
                    curly_count -= 1;
                } else {
                    // Somehow we ended up with unbalanced braces, abort
                    return fail_out(i, ParseErrorKind::UnternimatedVariable);
                }
            }
            ',' => {
                if curly_count == 0 && paren_count == 0 {
                    // We're outside any variable references, split just before the comma
                    return Ok(i.take_split(j));
                }
            }
            _ => {}
        }
    }

    Ok(i.take_split(i.len()))
}

fn strip<'a>(
    i: BlockSpan<'a>,
    start_location: Location,
) -> IResult<BlockSpan<'a>, AstNode, ParseErrorKind> {
    let (i, arg) = function_argument(i)?;
    if i.len() != 0 {
        return fail_out(i, ParseErrorKind::ExtraArguments("strip"));
    }

    let (_, arg) = parse_ast(arg)?;

    Ok((i, ast::strip(start_location, arg)))
}

fn words<'a>(
    i: BlockSpan<'a>,
    start_location: Location,
) -> IResult<BlockSpan<'a>, AstNode, ParseErrorKind> {
    let (i, arg) = function_argument(i)?;
    if i.len() != 0 {
        return fail_out(i, ParseErrorKind::ExtraArguments("words"));
    }

    let (_, arg) = parse_ast(arg)?;

    Ok((i, ast::words(start_location, arg)))
}

fn word<'a>(
    i: BlockSpan<'a>,
    start_location: Location,
) -> IResult<BlockSpan<'a>, AstNode, ParseErrorKind> {
    let (i, index) = function_argument(i)?;
    let (i, _) = match char!(i, ',') {
        Ok(v) => v,
        Err(_) => return fail_out(i, ParseErrorKind::InsufficientArguments("word")),
    };
    let (_, index) = parse_ast(index)?;

    let (i, list) = function_argument(i)?;
    if i.len() != 0 {
        return fail_out(i, ParseErrorKind::ExtraArguments("word"));
    }
    let (_, list) = parse_ast(list)?;

    Ok((i, ast::word(start_location, index, list)))
}

//! Logic for parsing conditional statements

// #SPC-P-Conditional

use super::ParserCompliance;
use super::{makefile_line, makefile_whitespace};
use crate::evaluated::{Block, BlockSpan};
use crate::ParseErrorKind;
use nom::{Context, Err, ErrorKind, IResult};
use std::sync::Arc;

#[cfg(test)]
mod test;

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Conditional {
    IfDef(Arc<Block>),
    IfNDef(Arc<Block>),
    IfEq(Arc<Block>, Arc<Block>),
    IfNEq(Arc<Block>, Arc<Block>),
    Else(Option<Box<Conditional>>),
    EndIf,
}

/// Parse a conditional line
/// TODO: allow configuring parse compliance mode
pub(super) fn parse_line<'a>(
    i: BlockSpan<'a>,
) -> IResult<BlockSpan<'a>, Conditional, ParseErrorKind> {
    let start_i = i;
    let (i, line) = makefile_line(i, ParserCompliance::GNU, false)?;

    match parse_line_internal(line.span()) {
        Ok((_, c)) => Ok((i, c)),
        Err(Err::Incomplete(_)) => Err(Err::Failure(Context::Code(
            i,
            ErrorKind::Custom(ParseErrorKind::ConditionalExpected),
        ))),
        Err(Err::Error(e)) => Err(super::lift_collapsed_span_error(Err::Error(e), start_i)),
        Err(Err::Failure(e)) => Err(super::lift_collapsed_span_error(Err::Failure(e), start_i)),
    }
}

/// Internal parser of the makefile line. Maybe this should be exposed at a high
/// level to avoid duplicate work computing the collapsed line?
fn parse_line_internal<'a>(
    input: BlockSpan<'a>,
) -> IResult<BlockSpan<'a>, Conditional, ParseErrorKind> {
    let (input, tag) = preceded!(
        input,
        makefile_whitespace,
        add_return_error!(
            ErrorKind::Custom(ParseErrorKind::ConditionalExpected),
            fix_error!(
                ParseErrorKind,
                alt_complete!(
                    tag_no_case!("ifdef")
                        | tag_no_case!("ifndef")
                        | tag_no_case!("ifeq")
                        | tag_no_case!("ifneq")
                        | tag_no_case!("else")
                        | tag_no_case!("endif")
                )
            )
        )
    )?;

    let tag = tag.into_string();
    let tag = tag.as_str();
    eprintln!("Tag is {:?}", tag);
    eprintln!("Rest is {:?}", input.into_string());

    match tag {
        "ifdef" | "ifndef" => {
            // #SPC-P-Conditional.ifdef
            let (input, rest) = return_error!(
                input,
                ErrorKind::Custom(ParseErrorKind::MalformedIfDef),
                preceded!(makefile_whitespace, fix_error!(ParseErrorKind, nom::rest))
            )?;

            if tag == "ifdef" {
                Ok((input, Conditional::IfDef(rest.to_new_block())))
            } else {
                // #SPC-P-Conditional.ifndef
                Ok((input, Conditional::IfNDef(rest.to_new_block())))
            }
        }
        "ifeq" | "ifneq" => {
            // TODO(bobtwinkles): The quality of the errors added to the error stack here are quite low
            let (input, (arg1, arg2)) = parse_ifeq(input)?;

            if tag == "ifeq" {
                Ok((
                    input,
                    Conditional::IfEq(arg1.to_new_block(), arg2.to_new_block()),
                ))
            } else {
                // #SPC-P-Conditional.ifneq
                Ok((
                    input,
                    Conditional::IfNEq(arg1.to_new_block(), arg2.to_new_block()),
                ))
            }
        }
        "else" => {
            let (input, sub_cond) = parse_else(input)?;
            Ok((input, Conditional::Else(sub_cond)))
        }
        "endif" => {
            let (input, _) = makefile_whitespace(input)?;
            if input.len() > 0 {
                Err(Err::Failure(Context::Code(
                    input,
                    ErrorKind::Custom(ParseErrorKind::ExtraTokensAfter("endif")),
                )))
            } else {
                Ok((input, Conditional::EndIf))
            }
        }
        _ => unreachable!(),
    }
}

/// #SPC-P-Conditional.ifeq
fn parse_ifeq<'a>(
    line: BlockSpan<'a>,
) -> IResult<BlockSpan<'a>, (BlockSpan<'a>, BlockSpan<'a>), ParseErrorKind> {
    eprintln!("Parsing ifeq line: {:?}", line);
    fn take_till_terminator<'a, 'b>(
        line: BlockSpan<'a>,
        terminator: char,
    ) -> IResult<BlockSpan<'a>, BlockSpan<'a>, ParseErrorKind> {
        if terminator == ',' || terminator == ')' {
            use nom::{InputIter, Slice};
            let mut count = 0i32;
            let mut iter = line.iter_indices();
            let (i, _) = match iter.find(|(_, c)| {
                let c = *c;
                if c == '(' {
                    count = count + 1;
                    false
                } else if c == ')' {
                    count = count - 1;
                    c == terminator && count < 0
                } else {
                    c == terminator && count <= 0
                }
            }) {
                Some(x) => x,
                None => {
                    return Err(Err::Error(error_position!(
                        line,
                        ErrorKind::Custom(ParseErrorKind::BadIfEqSeparator)
                    )));
                }
            };
            let res = line.slice(..i);
            let line = line.slice(i..);
            let (line, _) = fix_error!(line, ParseErrorKind, char!(terminator))?;

            Ok((line, res))
        } else {
            eprintln!("Taking until terminator {:?}", terminator);
            let res = fix_error!(
                line,
                ParseErrorKind,
                complete!(terminated!(
                    take_till!(|c| c == terminator),
                    char!(terminator)
                ))
            );
            eprintln!("we got {:?}", res);
            res
        }
    }

    let (line, terminator) = fix_error!(
        line,
        ParseErrorKind,
        complete!(alt_complete!(
            // #SPC-P-Conditional.ifeq_parens
            char!('(')  => {|_| (',')} |
            // #SPC-P-Conditional.ifeq_quotes
            char!('"')  => {|_| ('"')} |
            char!('\'') => {|_| ('\'')}
        ))
    )?;
    eprintln!("Using arg1 separator {:?}", terminator);

    let (line, arg1) = take_till_terminator(line, terminator)?;
    eprintln!("Got arg1 {:?}", arg1);
    let (line, _) = makefile_whitespace(line)?;
    let (line, terminator) = if terminator == ',' {
        (line, ')')
    } else {
        fix_error!(
            line,
            ParseErrorKind,
            complete!(alt_complete!(
                char!('"')  => {|_| ('"')} |
                char!('\'') => {|_| ('\'')}
            ))
        )?
    };
    let (line, arg2) = take_till_terminator(line, terminator)?;

    Ok((line, (arg1, arg2)))
}

// #SPC-P-Conditional.else
#[inline]
fn parse_else<'a>(
    input: BlockSpan<'a>,
) -> IResult<BlockSpan<'a>, Option<Box<Conditional>>, ParseErrorKind> {
    let (input, _) = makefile_whitespace(input)?;
    if input.len() > 0 {
        // There is something non-whitespace, so we *must* find another conditional
        let (input, cond) = return_error!(
            input,
            ErrorKind::Custom(ParseErrorKind::ExtraTokensAfter("else")),
            parse_line_internal
        )?;

        match cond {
            Conditional::IfDef(_)
            | Conditional::IfNDef(_)
            | Conditional::IfEq(_, _)
            | Conditional::IfNEq(_, _) => {
                // All of these are OK
            }
            _ => {
                // anything else is an error
                return Err(Err::Failure(Context::Code(
                    input,
                    ErrorKind::Custom(ParseErrorKind::ExtraTokensAfter("else")),
                )));
            }
        }

        Ok((input, Some(Box::new(cond))))
    } else {
        Ok((input, None))
    }
}

//! Logic for parsing conditional statements

// #SPC-P-Conditional

use super::makefile_whitespace;
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

impl<'a> crate::parsers::ParserState<'a> {
    /// Handle a conditional that came back from line parsing
    pub(super) fn handle_conditional(&mut self, conditional: Conditional) {
        unimplemented!("Handling conditional {:?}", conditional);
    }
}

/// Parse a conditional line
pub(super) fn parse_line<'a>(
    i: BlockSpan<'a>,
) -> IResult<BlockSpan<'a>, Conditional, ParseErrorKind> {
    match parse_line_internal(i) {
        Ok((i, c)) => Ok((i, c)),
        Err(Err::Incomplete(_)) => Err(Err::Failure(Context::Code(
            i,
            ErrorKind::Custom(ParseErrorKind::ConditionalExpected),
        ))),
        e => e,
    }
}

/// Internal parser of the makefile line. Maybe this should be exposed at a high
/// level to avoid duplicate work computing the collapsed line?
fn parse_line_internal<'a>(
    input: BlockSpan<'a>,
) -> IResult<BlockSpan<'a>, Conditional, ParseErrorKind> {
    #[derive(Copy, Clone, Debug, PartialEq, Eq)]
    enum ConditionalType {
        IfDef,
        IfNDef,
        IfEq,
        IfNEq,
        Else,
        EndIf,
    }
    let (input, tag) = preceded!(
        input,
        makefile_whitespace,
        add_return_error!(
            ErrorKind::Custom(ParseErrorKind::ConditionalExpected),
            terminated!(
                fix_error!(
                    ParseErrorKind,
                    alt_complete!(
                        tag_no_case!("ifdef") => { |_| ConditionalType::IfDef } |
                        tag_no_case!("ifndef") => { |_| ConditionalType::IfNDef } |
                        tag_no_case!("ifeq") => { |_| ConditionalType::IfEq } |
                        tag_no_case!("ifneq") => { |_| ConditionalType::IfNEq } |
                        tag_no_case!("else") => { |_| ConditionalType::Else } |
                        tag_no_case!("endif") => { |_| ConditionalType::EndIf }
                    )
                ),
                makefile_whitespace
            )
        )
    )?;

    eprintln!("Tag is {:?}", tag);
    eprintln!("Rest is {:?}", input.into_string());

    match tag {
        ConditionalType::IfDef | ConditionalType::IfNDef => {
            // #SPC-P-Conditional.ifdef
            let (input, rest) = return_error!(
                input,
                ErrorKind::Custom(ParseErrorKind::MalformedIfDef),
                fix_error!(ParseErrorKind, nom::rest)
            )?;

            if tag == ConditionalType::IfDef {
                Ok((input, Conditional::IfDef(rest.to_new_block())))
            } else {
                // #SPC-P-Conditional.ifndef
                Ok((input, Conditional::IfNDef(rest.to_new_block())))
            }
        }
        ConditionalType::IfEq | ConditionalType::IfNEq => {
            // TODO(bobtwinkles): The quality of the errors added to the error stack here are quite low
            let (input, (arg1, arg2)) = parse_ifeq(input)?;

            if tag == ConditionalType::IfEq {
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
        ConditionalType::Else => {
            let (input, sub_cond) = parse_else(input)?;
            Ok((input, Conditional::Else(sub_cond)))
        }
        ConditionalType::EndIf => {
            if input.len() > 0 {
                Err(Err::Failure(Context::Code(
                    input,
                    ErrorKind::Custom(ParseErrorKind::ExtraTokensAfter("endif")),
                )))
            } else {
                Ok((input, Conditional::EndIf))
            }
        }
    }
}

/// #SPC-P-Conditional.ifeq
fn parse_ifeq<'a>(
    line: BlockSpan<'a>,
) -> IResult<BlockSpan<'a>, (BlockSpan<'a>, BlockSpan<'a>), ParseErrorKind> {
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

    eprintln!("Parsing ifeq line: {:?}", line);

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

    eprintln!("ifeq parse succeeded");
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

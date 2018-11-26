//! Logic for parsing conditional statements
use super::{makefile_line, makefile_whitespace};
use super::{CollapsedLine, CollapsedLineSpan, ParserCompliance};
use crate::parsers::error_utils::NomErrExt;
use crate::{ParseErrorKind, Span};
use nom::{Context, Err, ErrorKind, IResult};

#[cfg(test)]
mod test;

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Conditional<'a> {
    IfDef(CollapsedLine<'a>),
    IfNDef(CollapsedLine<'a>),
    IfEq(CollapsedLine<'a>, CollapsedLine<'a>),
    IfNEq(CollapsedLine<'a>, CollapsedLine<'a>),
    Else(Option<Box<Conditional<'a>>>),
    EndIf,
}

/// Parse a conditional line
/// TODO: allow configuring parse compliance mode
pub(super) fn parse_line<'a>(i: Span<'a>) -> IResult<Span<'a>, Conditional<'a>, ParseErrorKind> {
    let (i, line) = makefile_line(i, ParserCompliance::GNU, false)?;

    match parse_line_internal(line.as_span()) {
        Ok((_, c)) => Ok((i, c)),
        Err(Err::Incomplete(_)) => Err(Err::Failure(Context::Code(
            i,
            ErrorKind::Custom(ParseErrorKind::ConditionalExpected),
        ))),
        Err(Err::Error(e)) => Err(super::lift_collapsed_span_error(Err::Error(e))),
        Err(Err::Failure(e)) => Err(super::lift_collapsed_span_error(Err::Failure(e))),
    }
}

/// Internal parser of the makefile line. Maybe this should be exposed at a high
/// level to avoid duplicate work computing the collapsed line?
fn parse_line_internal<'a, 'line: 'a>(
    input: CollapsedLineSpan<'a, 'line>,
) -> IResult<CollapsedLineSpan<'a, 'line>, Conditional<'line>, ParseErrorKind> {
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

    let tag = tag.flatten();
    let tag = tag.as_str();
    eprintln!("Tag is {:?}", tag);
    eprintln!("Rest is {:?}", input.flatten());

    match tag {
        "ifdef" | "ifndef" => {
            let (input, rest) = return_error!(
                input,
                ErrorKind::Custom(ParseErrorKind::MalformedIfDef),
                preceded!(makefile_whitespace, fix_error!(ParseErrorKind, nom::rest))
            )?;

            if tag == "ifdef" {
                Ok((input, Conditional::IfDef(rest.into())))
            } else {
                Ok((input, Conditional::IfNDef(rest.into())))
            }
        }
        "ifeq" | "ifneq" => {
            // TODO(bobtwinkles): The quality of the errors added to the error stack here are quite low
            let (input, (arg1, arg2)) = parse_ifeq(input)?;

            if tag == "ifeq" {
                Ok((input, Conditional::IfEq(arg1.into(), arg2.into())))
            } else {
                Ok((input, Conditional::IfNEq(arg1.into(), arg2.into())))
            }
        }
        "else" => {
            let (input, sub_cond) = parse_else(input)?;
            Ok((input, Conditional::Else(sub_cond)))
        }
        "endif" => {
            let (input, _) = makefile_whitespace(input)?;
            if input.length > 0 {
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

fn parse_ifeq<'a, 'line: 'a>(
    line: CollapsedLineSpan<'a, 'line>,
) -> IResult<
    CollapsedLineSpan<'a, 'line>,
    (CollapsedLineSpan<'a, 'line>, CollapsedLineSpan<'a, 'line>),
    ParseErrorKind,
> {
    let (line, terminator) = fix_error!(
        line,
        ParseErrorKind,
        complete!(alt_complete!(
                char!('"')  => {|_| ('"')} |
                char!('(')  => {|_| (',')} |
                char!('\'') => {|_| ('\'')}
            ))
    )?;

    fn take_till_terminator<'a, 'b>(
        line: CollapsedLineSpan<'a, 'b>,
        terminator: char,
    ) -> IResult<CollapsedLineSpan<'a, 'b>, CollapsedLineSpan<'a, 'b>, ParseErrorKind> {
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
                    )))
                }
            };
            let res = line.slice(..i);
            let line = line.slice(i..);
            let (line, _) = fix_error!(line, ParseErrorKind, char!(terminator))?;

            Ok((line, res))
        } else {
            fix_error!(
                line,
                ParseErrorKind,
                complete!(terminated!(
                    take_till!(|c| c == terminator),
                    char!(terminator)
                ))
            )
        }
    }

    let (line, arg1) = take_till_terminator(line, terminator)?;
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

#[inline]
fn parse_else<'a, 'line: 'a>(
    input: CollapsedLineSpan<'a, 'line>,
) -> IResult<CollapsedLineSpan<'a, 'line>, Option<Box<Conditional<'line>>>, ParseErrorKind> {
    eprintln!("doin the do");
    let (input, _) = makefile_whitespace(input)?;
    if input.length > 0 {
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

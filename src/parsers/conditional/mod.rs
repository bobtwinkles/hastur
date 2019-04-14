//! Logic for parsing conditional statements

// #SPC-P-Conditional

use super::makefile_whitespace;
use crate::ast::AstNode;
use crate::evaluated::BlockSpan;
use crate::parsers::ast::parse_ast;
use crate::ParseErrorKind;
use nom::{Context, Err, ErrorKind, IResult};

#[cfg(test)]
mod test;

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Conditional {
    IfDef(AstNode),
    IfNDef(AstNode),
    IfEq(AstNode, AstNode),
    IfNEq(AstNode, AstNode),
    Else(Option<Box<Conditional>>),
    EndIf,
}

impl Conditional {
    fn is_inverted(&self) -> bool {
        match *self {
            Conditional::IfNDef(_) => true,
            Conditional::IfNEq(_, _) => true,
            _ => false,
        }
    }
}

impl crate::parsers::ParserState {
    /// Handle a conditional that came back from line parsing
    pub(super) fn handle_conditional(
        &mut self,
        conditional: Conditional,
        names: &mut crate::NameCache,
        engine: &mut crate::Engine,
    ) -> Result<(), ParseErrorKind> {
        let conditional_is_inverted = conditional.is_inverted();
        match conditional {
            Conditional::IfEq(a, b) | Conditional::IfNEq(a, b) => {
                let a = a.eval(names, engine);
                let b = b.eval(names, engine);

                let a = a.into_string();
                let b = b.into_string();
                let a = a.trim();
                let b = b.trim();

                let conditional_mode = if a == b {
                    super::ConditionalInterpretation::Executing
                } else {
                    super::ConditionalInterpretation::NotExecuting
                };
                let conditional_mode = if conditional_is_inverted {
                    conditional_mode.invert()
                } else {
                    conditional_mode
                };

                self.conditionals.push(super::ConditionalState {
                    interpretation: conditional_mode,
                    seen_else: false,
                });
                self.update_ignoring();

                Ok(())
            }
            Conditional::Else(followup) => match followup {
                None => {
                    if self.conditionals.len() == 0 {
                        return Err(ParseErrorKind::UnattachedElse);
                    }
                    let last_idx = self.conditionals.len() - 1;
                    let top_conditional = &mut self.conditionals[last_idx];
                    if top_conditional.seen_else {
                        return Err(ParseErrorKind::TooManyElses);
                    }
                    top_conditional.interpretation = top_conditional.interpretation.invert();
                    top_conditional.seen_else = true;
                    self.update_ignoring();
                    Ok(())
                }
                Some(conditional) => {
                    unimplemented!("Handling `else` with followup {:?}", conditional)
                }
            },
            Conditional::EndIf => {
                if self.conditionals.len() == 0 {
                    return Err(ParseErrorKind::UnattachedEndIf);
                }
                self.conditionals.pop();

                Ok(())
            }
            conditional => unimplemented!("Handling conditional {:?}", conditional),
        }
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

    debug!("Tag is {:?}", tag);
    debug!("Rest is {:?}", input.into_string());

    match tag {
        ConditionalType::IfDef | ConditionalType::IfNDef => {
            // #SPC-P-Conditional.ifdef
            let (input, rest) = return_error!(
                input,
                ErrorKind::Custom(ParseErrorKind::MalformedIfDef),
                parse_ast
            )?;

            if tag == ConditionalType::IfDef {
                Ok((input, Conditional::IfDef(rest)))
            } else {
                // #SPC-P-Conditional.ifndef
                Ok((input, Conditional::IfNDef(rest)))
            }
        }
        ConditionalType::IfEq | ConditionalType::IfNEq => {
            // TODO(bobtwinkles): The quality of the errors added to the error stack here are quite low
            let (input, (arg1, arg2)) = parse_ifeq(input)?;

            if tag == ConditionalType::IfEq {
                Ok((input, Conditional::IfEq(arg1, arg2)))
            } else {
                // #SPC-P-Conditional.ifneq
                Ok((input, Conditional::IfNEq(arg1, arg2)))
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
) -> IResult<BlockSpan<'a>, (AstNode, AstNode), ParseErrorKind> {
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
            debug!("Taking until terminator {:?}", terminator);
            let res = fix_error!(
                line,
                ParseErrorKind,
                complete!(terminated!(
                    take_till!(|c| c == terminator),
                    char!(terminator)
                ))
            );
            debug!("we got {:?}", res);
            res
        }
    }

    debug!("Parsing ifeq line: {:?}", line);

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
    debug!("Using arg1 separator {:?}", terminator);

    let (line, arg1) = take_till_terminator(line, terminator)?;
    let (_, arg1) = parse_ast(arg1)?;
    debug!("Got arg1 {:?}", arg1);
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
    let (_, arg2) = parse_ast(arg2)?;

    debug!("ifeq parse succeeded");
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

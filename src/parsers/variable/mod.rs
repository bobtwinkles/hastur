//! Parser for variable definitions
use super::{makefile_line, makefile_whitespace};
use super::{CollapsedLine, CollapsedLineSpan, ParserCompliance};
use crate::{ParseErrorKind, Span, Variable};
use nom::{Err, ErrorKind, IResult};

#[cfg(test)]
mod test;

pub(super) fn parse_line<'a>(i: Span<'a>) -> IResult<Span<'a>, Variable, ParseErrorKind> {
    unimplemented!("unimplemented: variable parsing");
}

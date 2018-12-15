use crate::{Engine, ParseErrorKind, Span};
use nom::Err as NErr;
use nom::IResult;

mod collapsed_line;
mod comment;
mod conditional;
mod error_utils;
mod recipe_line;

use self::error_utils::lift_collapsed_span_error;
use self::recipe_line::recipe_line;

/// Whether this conditional allows or disallows interpretation of the makefile lines currently.
/// the moral equivalent of this enum (the char *ignoring in the conditional code for GNU make)
/// has 3 possible states, which is why this is an enum instead of just a bool. I don't know why
/// they need 3 states.
#[derive(Clone, Debug, PartialEq)]
enum ConditionalInterpretation {
    /// According to this conditional, lines should be executed
    AllowExecute,
    /// We should definitely ignore everything because the conditional evaluated to false
    DefinitelyIgnore,
}

/// Maintains the state of a conditional (`if{,n}{eq,def}`) while parsing
#[derive(Clone, Debug, PartialEq)]
struct ConditionalState {
    interpretation: ConditionalInterpretation,
    seen_else: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct ParserState<'a> {
    /// The current file name
    file_name: &'a str,

    /// Stack of conditionals
    conditionals: Vec<ConditionalState>,

    /// True when most lines should be ignored.
    /// This is used to avoid interpreting lines between ifeq/ifneq/ifdef/ifndef/else
    /// directives and their ending endif
    ignoring: bool,

    /// The currently open rule specification, if there is one.
    current_rule: Option<crate::Rule>,
}

impl<'a> ParserState<'a> {
    pub(crate) fn new(file_name: &'a str) -> ParserState {
        ParserState {
            file_name,
            conditionals: Vec::new(),
            ignoring: false,
            current_rule: None,
        }
    }

    /// Returns true if there is currently a rule open
    /// That is, if we've seen a target/dep line and nothing has caused us to
    /// dump the rule out to the database yet.
    fn currently_processing_rule(&self) -> bool {
        self.current_rule.is_some()
    }

    /// Close out the currently open rule, if there is one
    fn close_rule(&mut self, engine: &mut Engine) {
        let current_rule = std::mem::replace(&mut self.current_rule, None);
        match current_rule {
            Some(rule) => engine.add_rule(rule),
            None => {}
        }
    }
}

/// Structure that actually parses makefile lines
pub(crate) struct LineParser<'line, 'engine: 'line> {
    /// The engine context for the parse
    engine: &'line mut crate::Engine,

    /// The current parser state
    state: &'line mut ParserState<'engine>,
}

/// Represents the result of parsing a makefile line
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum MakefileLine<'a> {
    Comment,
    NewCommand(String),
    Conditional(conditional::Conditional<'a>),
}

/// Parse a makefile line
impl<'line, 'engine: 'line> LineParser<'line, 'engine> {
    pub(crate) fn new(
        engine: &'line mut crate::Engine,
        state: &'line mut ParserState<'engine>,
    ) -> Self {
        Self { engine, state }
    }

    /// The parser entry point
    pub(crate) fn parse_line(
        &'line mut self,
        i: Span<'engine>,
    ) -> IResult<Span<'engine>, MakefileLine<'engine>, ParseErrorKind> {
        /// A small macro that returns on Err(NErr::Failure) or Ok()
        /// but "backtracks" (ignores the result) on recoverable failures or
        /// incomplete
        macro_rules! run_parser(
            ($parsed:expr, $mfc:expr) => {
                match $parsed {
                    Ok((i, o)) => return Ok((i, $mfc(o))),
                    Err(NErr::Incomplete(_)) => {
                        debug!(
                            "Parser {:?} was incomplete, backtracking",
                            stringify!($parsed)
                        )
                    }
                    Err(NErr::Error(e)) => {
                        debug!(
                            "Parser {:?} emitted a recoverable error {:?}, backtracking",
                            stringify!($parsed),
                            e
                        )
                    }
                    Err(NErr::Failure(c)) => return Err(NErr::Failure(c)),
                }
            }
        );

        // First, check to see if we're about to try and parse a comment
        // TODO: validate that this is actually always an OK way to handle this,
        // since it diverges slightly from the GNU make behavior (which has
        // bespoke comment processing sprinkled through every other case)
        run_parser!(comment::parse_comment_following_whitespace(i), |_| {
            MakefileLine::Comment
        });

        if self.state.currently_processing_rule() {
            // We only check for command lines when we can reasonably expect
            // that we're currently processing a recipe
            run_parser!(recipe_line(i, self.engine.command_char), |line| {
                MakefileLine::NewCommand(line)
            });
        }

        run_parser!(conditional::parse_line(i), |o| MakefileLine::Conditional(o));

        if self.state.ignoring {
            // The parse state indicates that we should just ignore this line
            // We need to also grab its continuations
            // let (mut new_i, line) =
        }

        // TODO: Variable assignment here

        unimplemented!()
    }
}

/// Why did the line grab stop?
#[derive(Clone, Copy, Debug, PartialEq)]
enum LineEndReason {
    Comment,
    LineBreak,
    EOF,
}

named!(
    makefile_grab_line<Span, (Span, LineEndReason), ParseErrorKind >,
    fix_error!(ParseErrorKind, alt_complete!(
        take_until!("#")               => { |l| (l, LineEndReason::Comment) }  |  // Comments
        take_until_and_consume!("\n")  => { |l| (l, LineEndReason::LineBreak) } | // UNIX line endings
        take_until_and_consume!("\r\n")=> { |l| (l, LineEndReason::LineBreak) } | // Windows line endings
        nom::rest                      => { |l| (l, LineEndReason::EOF) }         // EOF
    ))
);

named!(
    makefile_comment,
    preceded!(nom::multispace0, preceded!(char!('#'), nom::rest))
);

/// Matches all makefile whitespace
#[inline]
fn makefile_whitespace<T: nom::InputTakeAtPosition<Item = char>>(
    input: T,
) -> IResult<T, T, ParseErrorKind> {
    fix_error!(input, ParseErrorKind, take_till!(|c| c != ' ' && c != '\t'))
}

/// Controls various aspects of the parser, making it conform to either the GNU
/// conventions or have strict POSIX compliance
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ParserCompliance {
    /// GNU compliance mode
    GNU,
    /// POSIX compliance mode
    POSIX,
}

/// A collapsed line of makefile input. Not for direct use because lifetimes are hard
#[derive(Clone, Debug, PartialEq)]
pub(crate) struct CollapsedLine<'a> {
    data: Vec<Span<'a>>,
    total_bytes: usize,
    // TODO: We can get better perf by caching the character counts at the start of each line
}

impl<'a> CollapsedLine<'a> {
    fn new(data: Vec<Span<'a>>) -> Self {
        let total_bytes = data.iter().map(|x| x.fragment.len()).sum();
        CollapsedLine { data, total_bytes }
    }

    /// Creates a single span representing this entire line
    pub(crate) fn as_span<'b>(&'b self) -> CollapsedLineSpan<'b, 'a> {
        CollapsedLineSpan {
            parent: &self.data,
            offset: 0,
            length: self.total_bytes,
        }
    }
}

impl<'a> std::ops::Deref for CollapsedLine<'a> {
    type Target = Vec<Span<'a>>;

    fn deref(&self) -> &Vec<Span<'a>> {
        &self.data
    }
}

/// Represents a subsequence of a collapsed line
#[derive(Copy, Clone, Debug, PartialEq)]
pub(crate) struct CollapsedLineSpan<'a, 'line> {
    /// The line we point into
    parent: &'a [Span<'line>],
    /// Offset in bytes from the start of the first line in `parent`
    offset: usize,
    /// length in bytes
    length: usize,
}

impl<'a, 'line: 'a> CollapsedLineSpan<'a, 'line> {
    /// Create an empty line span.
    fn empty() -> Self {
        Self {
            parent: &[],
            offset: 0,
            length: 0,
        }
    }

    /// Should not be used in normal code. Prefer `CollapsedLine::as_span` instead
    #[cfg(test)]
    pub(crate) fn new_test(parent: &'a [Span<'line>]) -> Self {
        let length = parent.iter().map(|x| x.fragment.len()).sum();
        Self {
            parent,
            offset: 0,
            length,
        }
    }

    /// Flatten this segment into a single string
    pub(crate) fn flatten(&self) -> String {
        use nom::InputIter;
        use std::iter::FromIterator;

        String::from_iter(self.iter_elements())
    }

    /// Get a reference to a span in the ballpark this collapsed span
    pub(crate) fn error_span(self) -> Span<'line> {
        use nom::Slice;
        if self.parent.len() > 0 {
            self.parent[0].slice(self.offset..)
        } else {
            Span::new(nom::types::CompleteStr("LOST"))
        }
    }

    /// Get the total length of this span in bytes
    pub(crate) fn len(&self) -> usize {
        self.length
    }
}

impl<'a, 'line: 'a> Into<CollapsedLine<'line>> for CollapsedLineSpan<'a, 'line> {
    // TODO(bobtwinkles): Write some tests for this
    fn into(mut self) -> CollapsedLine<'line> {
        use nom::Slice;
        let mut data = Vec::new();
        let mut iter = self.parent.into_iter();

        while let Some(parent_slice) = iter.next() {
            let mut to_push = *parent_slice;
            if self.offset != 0 {
                to_push = to_push.slice(self.offset..);
                self.offset = 0;
            }
            if self.length < to_push.fragment.len() {
                to_push = to_push.slice(..self.length);
            }
            self.length -= to_push.fragment.len();
            data.push(to_push)
        }
        return CollapsedLine::new(data);
    }
}

/// Get a line in the makefile. This does *not* behave correctly in the context
/// of building a command line or parsing a definition, since it implicitly
/// collapses line continuations. In GNU compliant mode, this means that we
/// don't do any munging other than replacing `"\\\n"` with ` `. POSIX compliant
/// Make implementations will replace all whitespace preceding and following a `"\\\n"`
/// pair with a single space character. We have no special handling for the
/// command character in either compliance mode.
/// TODO: rip out strip_initial_whitespace, we can do that smarter just using makefile_whitespace
fn makefile_line(
    mut i: Span,
    whitespace_handling: ParserCompliance,
    mut strip_initial_whitespace: bool,
) -> IResult<Span, CollapsedLine, ParseErrorKind> {
    /// Clean up the line end, depending on the parser compliance mode
    /// XXX: I'm not entirely convinced that the handling of whitespace is right here
    /// POSIX spec: http://pubs.opengroup.org/onlinepubs/9699919799/utilities/make.html
    /// GNU behavior: lol thinking GNU has a spec?
    #[inline(always)]
    fn clean_line_end(s: &mut Span, backslash_count: usize, whitespace_handling: ParserCompliance) {
        // If there's only one backslash, then removing it might expose some
        // whitespace we need to strip in POSIX mode
        let fragment = s.fragment.0;
        s.fragment.0 = if backslash_count == 1 {
            match whitespace_handling {
                ParserCompliance::POSIX => {
                    // In POSIX mode, we just drop the last character
                    // XXX: Validate that this is actually what a POSIX-compliant make does
                    fragment.get(..fragment.len() - 1).expect(
                        "Stripping one byte should be safe wen the line ends with a backslash",
                    )
                }
                ParserCompliance::GNU => {
                    if fragment.len() < 2 {
                        // Either the empty string or just "\\" should go to the empty string
                        fragment.get(0..0).unwrap()
                    } else {
                        // In GNU mode we strip off all the whitespace and the backslash
                        let end_byte = {
                            let bytes = s.fragment.0.as_bytes();
                            // Tracking the end of the characters we're stripping.
                            // This will be at least the end - 1, since we have a backslash
                            let mut end = if fragment.len() > 1 {
                                s.fragment.0.len() - 2
                            } else {
                                0
                            };
                            // Walk back through the string while end is still pointing to a space or tab
                            while end > 0 && (bytes[end] == 0x20 || bytes[end] == 0x09) {
                                end -= 1;
                            }

                            end + 1
                        };

                        fragment
                            .get(..end_byte)
                            .expect("Our walk should always end on a UTF8 character boundary")
                    }
                }
            }
        } else {
            // There is enough backslashes at the end to just strip off half of them and be done with it
            fragment.get(..(fragment.len() - backslash_count + (backslash_count / 2)))
                .expect("there should be enough backslashes for us to strip off the ceiling division by 2")
        }
    }

    let (new_i, mut first_line) = if strip_initial_whitespace {
        preceded!(i, makefile_whitespace, makefile_grab_line)?
    } else {
        makefile_grab_line(i)?
    };
    i = new_i;
    let mut lines = Vec::new();
    let mut backslash_count = ends_with_backslash(first_line.0.fragment.0);
    if backslash_count & 1 == 0 {
        // There is an even number of backslashes, so don't escape them
        lines.push(first_line.0);
        return Ok((i, CollapsedLine::new(lines)));
    }
    // Otherwise, we need to clean up the line
    clean_line_end(&mut first_line.0, backslash_count, whitespace_handling);
    if first_line.0.fragment.len() > 0 {
        // We found some real content, don't keep skipping whitespace
        strip_initial_whitespace = false;
        lines.push(first_line.0);
    }
    let mut line_end_reason = first_line.1;
    // Tracks whether or not we should add a space.
    // We add a space after consuming the first line, and after every line of non-zero length
    let mut should_add_space = true;
    loop {
        if line_end_reason != LineEndReason::Comment {
            if should_add_space && !strip_initial_whitespace {
                let mut span = Span::new(nom::types::CompleteStr(" "));
                span.line = i.line;
                span.offset = i.offset;

                lines.push(span);
            } else {
                // Reset should_add_space to make sure we check it again
                should_add_space = true;
            }
        } else {
            let mut span = Span::new(nom::types::CompleteStr("#"));
            span.line = i.line;
            span.offset = i.offset - 1;

            lines.push(span);
        }

        let (new_i, mut new_line) = match preceded!(i, makefile_whitespace, makefile_grab_line) {
            Ok(x) => x,
            Err(NErr::Incomplete(_)) => {
                debug!("Incomplete parse while trying to grab a full line");
                return Ok((i, CollapsedLine::new(lines)));
            }
            Err(NErr::Error(_)) => {
                debug!("Was not able to find more line while pulling makefile line");
                return Ok((i, CollapsedLine::new(lines)));
            }
            Err(NErr::Failure(e)) => {
                // propegate failures
                return Err(NErr::Failure(e));
            }
        };

        i = new_i;
        line_end_reason = new_line.1;
        backslash_count = ends_with_backslash(new_line.0.fragment.0);
        if backslash_count & 1 == 0 {
            lines.push(new_line.0);
            break;
        }
        clean_line_end(&mut new_line.0, backslash_count, whitespace_handling);
        if new_line.0.fragment.len() > 0 {
            // We found content so we can stop skipping whitespace
            strip_initial_whitespace = false;
            lines.push(new_line.0);
        } else {
            should_add_space = false;
        }
    }

    Ok((i, CollapsedLine::new(lines)))
}

/// Test if a line ends with a backslash, in a Makefile compatible way
pub(crate) fn ends_with_backslash(s: &str) -> usize {
    let mut backslash_count = 0;
    let mut i = s.len();
    let bytes = s.as_bytes();
    while i > 0 && bytes[i - 1] == 0x5C {
        backslash_count += 1;
        i = i - 1;
    }

    backslash_count
}

#[cfg(test)]
mod test;

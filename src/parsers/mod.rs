use crate::ast::AstNode;
use crate::evaluated::{Block, BlockSpan, ContentReference};
use crate::{Engine, NameCache, ParseErrorKind, Recipe, VariableName};
use nom::Err as NErr;
use nom::IResult;
use std::sync::Arc;

#[cfg(test)]
#[macro_use]
mod test;

#[macro_use]
mod macros;

pub(crate) mod ast;
mod comment;
mod conditional;
mod directives;
mod error_utils;
mod file_sequence;
mod recipe_line;
pub(crate) mod targets;
pub(crate) mod variable;

use self::error_utils::lift_collapsed_span_error;
use self::recipe_line::recipe_line;

/// Whether this conditional allows or disallows interpretation of the makefile lines currently.
/// the moral equivalent of this enum (the char *ignoring in the conditional code for GNU make)
/// has 3 possible states, which is why this is an enum instead of just a bool. I don't know why
/// they need 3 states.
#[derive(Copy, Clone, Debug, PartialEq)]
enum ConditionalInterpretation {
    /// According to this conditional, lines should be executed
    Executing,
    /// We should definitely ignore everything because the conditional evaluated to false
    NotExecuting,
}

impl ConditionalInterpretation {
    fn invert(self) -> Self {
        match self {
            ConditionalInterpretation::Executing => ConditionalInterpretation::NotExecuting,
            ConditionalInterpretation::NotExecuting => ConditionalInterpretation::Executing,
        }
    }
}

/// Maintains the state of a conditional (`if{,n}{eq,def}`) while parsing
#[derive(Clone, Debug, PartialEq)]
struct ConditionalState {
    interpretation: ConditionalInterpretation,
    seen_else: bool,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub(crate) struct DefineState {
    /// The variable name we're currently defining
    var: VariableName,

    // How deeply nested the defines are
    nesting: u32,
}

/// Contains the information that may come out of a rule line. Note that this is
/// not precisely the same as a `hastur::Rule`, since it can contain multiple
/// targets and thus result in the production of multiple rules.
#[derive(Clone, Debug, PartialEq)]
pub(crate) struct ProtoRule {
    /// The targets that this rule builds
    pub targets: Vec<Arc<Block>>,
    /// The inputs for the recipe
    pub deps: Vec<Arc<Block>>,
    /// The recipe to turn `deps` into `targets`
    pub recipe: Recipe,
    /// Extra information about the rule
    pub rule_type: crate::RuleType,
}

impl ProtoRule {
    /// Push a new line into the rule
    pub(super) fn push_command_line(&mut self, line: AstNode) {
        self.recipe.0.push(crate::Command {
            unexpanded_command: line,
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct ParserState {
    /// Stack of conditionals
    conditionals: Vec<ConditionalState>,

    /// True when most lines should be ignored.
    /// This is used to avoid interpreting lines between ifeq/ifneq/ifdef/ifndef/else
    /// directives and their ending endif
    ignoring: bool,

    /// The currently open rule specification, if there is one.
    current_rule: Option<ProtoRule>,

    /// Active define
    current_define: Option<DefineState>,
}

impl ParserState {
    pub(crate) fn new() -> ParserState {
        ParserState {
            conditionals: Vec::new(),
            ignoring: false,
            current_rule: None,
            current_define: None,
        }
    }

    /// Feed a line into the parser
    pub(crate) fn parse_line<'a>(
        &mut self,
        i: BlockSpan<'a>,
        names: &mut NameCache,
        engine: &mut Engine,
    ) -> IResult<BlockSpan<'a>, (), ParseErrorKind> {
        let line_start = i;
        /// A small macro that returns on Err(NErr::Failure) or Ok()
        /// but "backtracks" (ignores the result) on recoverable failures or
        /// incomplete
        macro_rules! run_parser(
            ($parsed:expr, $cleanup:expr) => {
                match $parsed {
                    Ok((i, o)) => match $cleanup(o) {
                        Ok(()) => return Ok((i, ())),
                        Err(e) => return fail_out(line_start, e)
                    },
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
                            e.into_error_kind()
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
            // Comment lines don't require any particular action
            info!("Skipping over comment line");
            Ok(())
        });

        if self.currently_processing_rule() {
            // We only check for command lines when we can reasonably expect
            // that we're currently processing a recipe
            run_parser!(recipe_line(i, engine.command_char), |line| {
                // Push the line. This is safe since we only run this parser if
                // we're currently processing a rule
                self.push_command_line(line);
                Ok(())
            });
        } else if self.current_define.is_some() {
            // If we're currently parsing a line inside a define, handle that
            run_parser!(variable::parse_define_line(i), |action| self
                .handle_define_line(engine, action));
            panic!("Parsing a define line should never fail");
        }

        // If it's not a recipe line, we can safely collapse all continuations
        // TODO: expose the parser compliance knob upstream
        let (i, line) = makefile_line(i, ParserCompliance::GNU, true)?;

        // Convenience macro for running parsers against the line
        macro_rules! run_line_parser(
            ($parsed:expr, $cleanup:expr) => {
                match $parsed {
                    Ok((_, o)) => match $cleanup(o) {
                        Ok(()) => return Ok((i, ())),
                        Err(e) => return fail_out(line_start, e)
                    },
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
                            e.into_error_kind()
                        )
                    }
                    Err(NErr::Failure(c)) => return Err(lift_collapsed_span_error(NErr::Failure(c), line_start)),
                }
            }
        );

        debug!("Parsing in collapsed line {:?}", line.into_string());
        // If the line is empty, just succeed immediately
        if line.len() == 0 {
            return Ok((i, ()));
        }

        run_line_parser!(conditional::parse_line(line.span()), |conditional| self
            .handle_conditional(conditional, names, engine));

        if self.ignoring {
            // The parse state indicates that we should just ignore this line
            // We've already collapsed continuations, so just return immediately
            return Ok((i, ()));
        }

        run_line_parser!(
            variable::parse_line(line.span(), names, engine),
            |variable_action| {
                debug!("Matched variable action {:?}", variable_action);
                // Successful variable assignments close the current rule
                self.close_rule(names, engine);

                self.handle_global_variable_action(engine, variable_action)
            }
        );

        run_line_parser!(directives::parse_line(line.span()), |directive_action| {
            self.close_rule(names, engine);
            self.handle_directive_action(names, engine, directive_action)
        });

        if line.span().chars().next() == Some(engine.command_char) {
            // A tab (command character) at this point is definitely an error.
            return fail_out(line_start, ParseErrorKind::RecipeExpected);
        }

        run_line_parser!(
            targets::parse_line(line.span(), names, engine),
            |target_action| {
                self.close_rule(names, engine);

                self.handle_target_action(engine, target_action)
            }
        );

        Ok((i, ()))
    }

    /// Update the internal ignoring state based on the conditional state and
    /// whether or not we are in a define
    fn update_ignoring(&mut self) {
        // If any conditional is suppressing interpretation, we must be ignoring
        for conditional in &self.conditionals {
            if conditional.interpretation == ConditionalInterpretation::NotExecuting {
                self.ignoring = true;
                return;
            }
        }
        // TODO: incorporate the in-define logic here

        // If there's no reason to ignore, we aren't
        self.ignoring = false;
    }

    /// Returns true if there is currently a rule open
    /// That is, if we've seen a target/dep line and nothing has caused us to
    /// dump the rule out to the database yet.
    fn currently_processing_rule(&self) -> bool {
        self.current_rule.is_some()
    }

    /// Push a new command line into the currently processing rule
    /// # Panics
    /// Panics if there no rule is currently active
    fn push_command_line(&mut self, line: crate::ast::AstNode) {
        self.current_rule
            .as_mut()
            .expect("cannot push command lines while not processing rules")
            .push_command_line(line);
    }

    /// Close out the currently open rule, if there is one
    fn close_rule(&mut self, names: &mut NameCache, engine: &mut Engine) {
        debug!("Closing rule {:?}", self.current_rule);
        let current_rule = std::mem::replace(&mut self.current_rule, None);
        match current_rule {
            Some(rule) => engine.from_protorule(names, rule),
            None => {}
        }
    }
}

/// Why did the line grab stop?
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LineEndReason {
    Comment,
    LineBreak,
    EOF,
}

/// Get a line, stopping on a UNIX line break, Windows line break, or the beginning of a comment
pub fn makefile_grab_line<T>(input: T) -> IResult<T, (T, LineEndReason), ParseErrorKind>
where
    T: Clone
        + Copy
        + nom::AtEof
        + nom::Slice<std::ops::Range<usize>>
        + nom::Slice<std::ops::RangeFrom<usize>>
        + nom::Slice<std::ops::RangeTo<usize>>
        + nom::InputTake
        + nom::InputIter<Item = char>
        + nom::Compare<&'static str>
        + nom::InputLength
        + nom::FindSubstring<&'static str>,
{
    fix_error!(
        input,
        ParseErrorKind,
        alt_complete!(
            take_until!("#") =>
                { |l| (l, LineEndReason::Comment) }  |  // Comments
            terminated!(take_until!("\n"), tag!("\n")) =>
                { |l| (l, LineEndReason::LineBreak) } | // UNIX line endings
            terminated!(take_until!("\r\n"), tag!("\r\n")) =>
                { |l| (l, LineEndReason::LineBreak) } | // Windows line endings
            nom::rest =>
                { |l| (l, LineEndReason::EOF) }         // EOF
        )
    )
}

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

/// Capture the next token in the input
#[inline]
pub fn makefile_token<T>(input: T) -> IResult<T, T, ParseErrorKind>
where
    T: Clone + nom::InputTakeAtPosition<Item = char>,
{
    pe_fix!(
        input,
        delimited!(
            makefile_whitespace,
            pe_fix!(take_while!(|c| c != ' ' && c != '\t')),
            makefile_whitespace
        )
    )
}

/// Find a character satisfying a predicate, unquoted by `\`, returning the preceding text
/// with all quoting '\' removed. Note that this DOES NOT remove `\`s in things
/// that do not immediately precede the character of interest. For example,
/// (assuming we're looking for an unquoted `%` character)the string `\\%` will
/// be turned into `\%`, with a match on the `%`. However, `\\c%` *will not*
/// remove either of the two `\` characters, and will still match on the `%`
/// character. Some further examples
///   - `\\\%` will not match at all, since the `%` is escaped.
///   - `%\\%` will match the first `%`, and leave the `\\%` untouched
///   - `\\\%.%` will match on the second `%`, returning `\%` as the preceding content.
pub fn makefile_take_until_unquote<'a, F>(
    i: BlockSpan<'a>,
    mut stop: F,
) -> (Arc<Block>, Option<(BlockSpan<'a>, BlockSpan<'a>)>)
where
    F: FnMut(char) -> bool,
{
    use nom::{InputIter, Slice};
    let mut tr = Block::new(i.parent().raw_sensitivity(), Vec::new());
    let mut stop_character = None;

    let mut last_nonslash_idx = -1;
    let mut next_push_start = 0;
    let mut stopchar_index = i.len() + 1;
    let mut stopchar_seen = false;
    for (idx, ch) in i.iter_indices() {
        if stop(ch) {
            stopchar_seen = true;
            debug!("Found stopchar at index {:?}", idx);
            // We found a stopchar. Push the previous span and figure out if it
            // actually matches
            stop_character = Some(i.slice(idx..idx + 1));

            // slash count examples:
            //  0 1 2 3
            //  a b %   => 2 - 1 - 1    = 0
            //  a b \ % => 3 - 1 - 1    = 1
            //  \ \ %   => 2 - (-1) - 1 = 2
            //  \ %     => 1 - (-1) - 1 = 1
            let slash_count = idx as isize - last_nonslash_idx - 1;

            let end_idx = (idx as isize - slash_count.checked_div(2).unwrap()) as usize;

            if slash_count % 2 == 1 {
                // Odd number of slashes. Continue on since this means that the
                // stopchar is escaped. Leave next_push_start setup to push the
                // stopchar for us, but push the backslashes immediately
                Arc::make_mut(&mut tr).push_all_contents(i.slice(next_push_start..end_idx - 1));
                next_push_start = idx;
            } else {
                // Even number of slashes. Push half of them, indicate that we
                // found the stopchar, and then break out
                Arc::make_mut(&mut tr).push_all_contents(i.slice(next_push_start..end_idx));
                stopchar_index = idx;
                break;
            }
        }
        if ch != '\\' {
            last_nonslash_idx = idx as isize;
        }
    }

    debug!("After search, stopchar at {:?}", stopchar_index);

    // Special case: matching of "null" characters at the end of input
    if stop('\0') && stopchar_index > i.len() {
        let idx = i.len();
        let slash_count = idx as isize - last_nonslash_idx - 1;

        let end_idx = (idx as isize - slash_count.checked_div(2).unwrap()) as usize;
        Arc::make_mut(&mut tr).push_all_contents(i.slice(next_push_start..end_idx));

        if slash_count % 2 == 1 {
            // Odd number of slashes. Leave next_push_start set up
            next_push_start = idx;
        } else {
            // Even number of slashes. Indicate that we found the stopchar
            stopchar_index = idx - 1;
        }
    }

    if stopchar_index < i.len() {
        let stop_character = stop_character.unwrap_or_else(|| i.slice(i.len()..));
        Arc::make_mut(&mut tr).simplify();
        (tr, Some((stop_character, i.slice(stopchar_index + 1..))))
    } else if stopchar_index == i.len() {
        let stop_character = stop_character.unwrap_or_else(|| i.slice(i.len()..));
        Arc::make_mut(&mut tr).simplify();
        (tr, Some((stop_character, i.slice(stopchar_index..))))
    } else {
        if !stopchar_seen {
            (i.to_new_block(), None)
        } else {
            Arc::make_mut(&mut tr).push_all_contents(i.slice(next_push_start..));
            Arc::make_mut(&mut tr).simplify();

            (tr, None)
        }
    }
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

/// Get a line in the makefile. This does *not* behave correctly in the context
/// of building a command line or parsing a definition, since it implicitly
/// collapses line continuations. In GNU compliant mode, this means that we
/// don't do any munging other than replacing `"\\\n"` with ` `. POSIX compliant
/// Make implementations will replace all whitespace preceding and following a `"\\\n"`
/// pair with a single space character. We have no special handling for the
/// command character in either compliance mode.
/// TODO: rip out strip_initial_whitespace, we can do that smarter just using makefile_whitespace
pub(crate) fn makefile_line(
    mut i: BlockSpan,
    whitespace_handling: ParserCompliance,
    mut strip_initial_whitespace: bool,
) -> IResult<BlockSpan, Arc<Block>, ParseErrorKind> {
    /// Clean up the line end, depending on the parser compliance mode
    /// XXX: I'm not entirely convinced that the handling of whitespace is right here
    /// POSIX spec: http://pubs.opengroup.org/onlinepubs/9699919799/utilities/make.html
    /// GNU behavior: lol thinking GNU has a spec?
    #[inline(always)]
    fn clean_line_end(
        s: &mut BlockSpan,
        backslash_count: usize,
        whitespace_handling: ParserCompliance,
    ) {
        use nom::Slice;
        // If there's only one backslash, then removing it might expose some
        // whitespace we need to strip in POSIX mode
        *s = if backslash_count == 1 {
            match whitespace_handling {
                ParserCompliance::POSIX => {
                    // In POSIX mode, we just drop the last character
                    // XXX: Validate that this is actually what a POSIX-compliant make does
                    s.slice(..s.len() - 1)
                }
                ParserCompliance::GNU => {
                    if s.len() < 2 {
                        // Either the empty string or just "\\" should go to the empty string
                        s.slice(0..0)
                    } else {
                        // In GNU mode we strip off all the whitespace and the backslash
                        let mut last_non_whitespace = 0;
                        for (i, c) in s.char_indices() {
                            if c == ' ' || c == '\t' || c == '\\' {
                                continue;
                            }
                            last_non_whitespace = i;
                        }

                        s.slice(..last_non_whitespace + 1)
                    }
                }
            }
        } else {
            // There is enough backslashes at the end to just strip off half of them and be done with it
            s.slice(..(s.len() - backslash_count + (backslash_count / 2)))
        }
    }

    let (new_i, mut first_line) = if strip_initial_whitespace {
        preceded!(i, makefile_whitespace, makefile_grab_line)?
    } else {
        makefile_grab_line(i)?
    };
    i = new_i;
    let mut lines: Vec<ContentReference> = Vec::new();
    let mut backslash_count = ends_with_backslash(first_line.0);
    if backslash_count & 1 == 0 {
        // There is an even number of backslashes, so don't escape them
        lines.push(first_line.0.to_content_reference());
        return Ok((i, Block::new(Default::default(), lines)));
    }
    // Otherwise, we need to clean up the line
    clean_line_end(&mut first_line.0, backslash_count, whitespace_handling);
    if first_line.0.len() > 0 {
        // We found some real content, don't keep skipping whitespace
        strip_initial_whitespace = false;
        lines.push(first_line.0.to_content_reference());
    }
    let mut line_end_reason = first_line.1;
    // Tracks whether or not we should add a space.
    // We add a space after consuming the first line, and after every line of non-zero length
    let mut should_add_space = true;
    loop {
        if line_end_reason != LineEndReason::Comment {
            if should_add_space && !strip_initial_whitespace {
                lines.push(ContentReference::space());
            } else {
                // Reset should_add_space to make sure we check it again
                should_add_space = true;
            }
        } else {
            lines.push(ContentReference::space());
        }

        let (new_i, mut new_line) = match preceded!(i, makefile_whitespace, makefile_grab_line) {
            Ok(x) => x,
            Err(NErr::Incomplete(_)) => {
                debug!("Incomplete parse while trying to grab a full line");
                return Ok((i, Block::new(Default::default(), lines)));
            }
            Err(NErr::Error(_)) => {
                debug!("Was not able to find more line while pulling makefile line");
                return Ok((i, Block::new(Default::default(), lines)));
            }
            Err(NErr::Failure(e)) => {
                // propegate failures
                return Err(NErr::Failure(e));
            }
        };

        i = new_i;
        line_end_reason = new_line.1;
        backslash_count = ends_with_backslash(new_line.0);
        if backslash_count & 1 == 0 {
            lines.push(new_line.0.to_content_reference());
            break;
        }
        clean_line_end(&mut new_line.0, backslash_count, whitespace_handling);
        if new_line.0.len() > 0 {
            // We found content so we can stop skipping whitespace
            strip_initial_whitespace = false;
            lines.push(new_line.0.to_content_reference());
        } else {
            should_add_space = false;
        }
    }

    Ok((i, Block::new(Default::default(), lines)))
}

/// Create a new Nom `IResult` recoverable error with our custom error kind
pub(crate) fn error_out<'a, T>(
    i: BlockSpan<'a>,
    err: ParseErrorKind,
) -> IResult<BlockSpan<'a>, T, ParseErrorKind> {
    Err(NErr::Error(nom::Context::Code(
        i,
        nom::ErrorKind::Custom(err),
    )))
}

/// Create a new Nom `IResult` unrecoverable error with our custom error kind
pub(crate) fn fail_out<'a, T>(
    i: BlockSpan<'a>,
    err: ParseErrorKind,
) -> IResult<BlockSpan<'a>, T, ParseErrorKind> {
    Err(NErr::Failure(nom::Context::Code(
        i,
        nom::ErrorKind::Custom(err),
    )))
}

/// Test if a line ends with a backslash, in a Makefile compatible way
pub(crate) fn ends_with_backslash(s: BlockSpan) -> usize {
    let mut backslash_count = 0;
    for c in s.chars() {
        if c == '\\' {
            backslash_count += 1;
        } else {
            backslash_count = 0
        }
    }
    backslash_count
}

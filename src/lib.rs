/* Hastur's main file
Copyright (C) 2018 Reed Koser
This file is NOT part of GNU Make.
GNU Make and Hastur are free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3 of the License, or (at your option) any
later version.

Hastur is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program.  If not, see <http://www.gnu.org/licenses/>.  */

//! This is Hastur, named after the god of shepherds from the Cthulhu mythos.
//! The primary purpose of this crate is to provide a way to evaluate statements
//! in GNU Make syntax.
#![warn(missing_docs)]

extern crate fxhash;
#[macro_use]
extern crate log;
#[macro_use]
extern crate nom;
extern crate nom_locate;

// #[cfg(test)]
// #[macro_use]
// extern crate lazy_static;

mod parsers;
pub mod pattern;

use fxhash::FxHashMap;
use std::io;
use std::io::prelude::*;
use std::path::PathBuf;

/// What sort of thing went wrong while parsing.
#[derive(Clone, Debug, PartialEq)]
pub enum ParseErrorKind {
    /// Generated when there are tokens after something we expect to be the end of a line
    ExtraTokensAfter(&'static str),
    /// Trying to do something like a free-floating "else" without and if, or too many endifs
    MismatchedBranchCommand,
    /// Trying to write something like
    /// ```makefile
    /// if
    /// else
    /// else
    /// ```
    TooManyElses,

    /// We expected to find a comment (something starting with #) but found something else
    CommentExpected,

    /// We expected a recipe line but didn't find one
    RecipeExpected,

    /// We expected a conditional but found something else
    ConditionalExpected,

    /// An ifeq/ifneq was encountered but had an illegal separator character
    BadIfEqSeparator,

    /// We couldn't grab the line for an `ifdef` for whatever reason. This also
    /// includes `ifndefs`.
    MalformedIfDef,

    /// We failed to parse an `ifeq` for some reason. This also includes `ifneq`s.
    MalformedIfEq,

    /// This variant should never surface in library consumer code in practice,
    /// but we need it to make nom types work out
    NomError(u32),
}

/// Represents a span of the input
/// TODO: we should really wrap this in our own structure so that nom_locate isn't part of
/// our public API
pub type Span<'a> = nom_locate::LocatedSpan<nom::types::CompleteStr<'a>>;

/// Represents an owned span that originated from the input
/// The `T` parameter is used to keep track of the originating file name.
/// Making it generic allows us to be agnostic to whether or not that particular
/// piece of information is owned. For most of the public API though, this will
/// simply be a string reference that the user themselves handed us.
#[derive(Clone, Debug, PartialEq)]
struct OwnedFragment<T> {
    /// The line on which this fragment starts
    pub line: u32,
    /// The offset of this fragment from the beginning of the input
    /// represented by the provided file name
    pub offset: usize,
    /// The column at which this fragment starts
    pub column: usize,
    /// The actual fragment of the input, as an owned string
    pub fragment: String,
    /// The file name from which this fragment originated
    pub file_name: T,
}

impl<T> OwnedFragment<T> {
    pub fn from_span(s: Span, file_name: T) -> OwnedFragment<T> {
        Self {
            line: s.line,
            offset: s.offset,
            column: s.get_column(),
            fragment: s.fragment.0.to_owned(),
            file_name,
        }
    }
}

/// A parse error
#[derive(Clone, Debug, PartialEq)]
pub struct ParseError<'a> {
    span: &'a Span<'a>,
    kind: ParseErrorKind,
}

impl<'a> ParseError<'a> {
    fn new(span: &'a Span<'a>, kind: ParseErrorKind) -> ParseError {
        ParseError { span, kind }
    }

    fn to_static(self) -> OwnedParseError {
        OwnedParseError {
            line: self.span.line,
            column: self.span.get_column(),
            fragment: self.span.fragment.0.to_owned(),
            kind: self.kind,
        }
    }
}

impl<'a> From<u32> for ParseErrorKind {
    fn from(other: u32) -> ParseErrorKind {
        ParseErrorKind::NomError(other)
    }
}

/// A parse error that owns all its information
pub struct OwnedParseError {
    /// The line at which the error occurred
    pub line: u32,
    /// The column within the line at which the error occurred
    pub column: usize,
    /// An owned string representing a copy of the fragment that caused this error
    pub fragment: String,
    /// What kind of error this is
    pub kind: ParseErrorKind,
}

/// A command.
/// We track only the unexpanded version of the command string, since
/// command variable expansion is not performed until after makfile reading
/// is complete. Thus, we need to have finished reading all Makefiles
/// before we can start interpreting these command strings
/// See [the GNU Make manual](https://www.gnu.org/software/make/manual/make.html#Recipe-Syntax)
/// for more details
#[derive(Clone, Debug, PartialEq)]
pub struct Command {
    unexpanded_command: String,
}

/// A "rule" describes how to bring a set of "targets" up to date from a set of
/// "dependencies", using a "recipe".
#[derive(Clone, Debug, PartialEq)]
pub struct Rule {
    /// The targets that this rule builds
    targets: Vec<String>,
    /// The inputs for the recipe
    deps: Vec<String>,
    /// The recipe to turn `deps` into `targets`
    recipe: Vec<Command>,
}

impl Rule {
    fn new(targets: Vec<String>, deps: Vec<String>) -> Self {
        Self {
            targets,
            deps,
            recipe: Vec::new(),
        }
    }
}

/// Represents a make variable.
/// Currently completely unimplemented
pub struct Variable {}

/// Database of rules and variables.
/// This represents the full context required to evaluate something in Makefile syntax
#[derive(Default)]
pub struct Database {
    variables: FxHashMap<String, Variable>,
    rules: Vec<Rule>,
}

impl Database {
    /// Add a rule into the database
    pub fn add_rule(&mut self, rule: Rule) {
        self.rules.push(rule);
    }

    /// Get a variable based on a name
    pub fn get_variable(&self, name: &str) -> Option<&Variable> {
        self.variables.get(name)
    }
}

/// Represents all the things that can go wrong while parsing an evaluating
/// makefile statements.
pub enum MakefileError {
    /// An IO error occurred while reading the makefile
    IOError(io::Error),
    /// The parser wasn't able to handle some of the syntax
    ParseError(OwnedParseError),
}

impl From<io::Error> for MakefileError {
    fn from(other: io::Error) -> Self {
        MakefileError::IOError(other)
    }
}

impl<'a> From<ParseError<'a>> for MakefileError {
    fn from(other: ParseError<'a>) -> Self {
        MakefileError::ParseError(other.to_static())
    }
}

/// Represents the state of the parsing engine
pub struct Engine {
    /// The database state.
    pub database: Database,

    /// The current command-prefix character.
    /// Defaults to `\t` (tab)
    pub command_char: char,

    /// The primary vpaths. These apply to all file types.
    /// This list is maintained in the search order (i.e. `vpaths[0]` is
    /// checked, then `vpaths[1]`, etc.)
    pub vpaths: Vec<PathBuf>,

    /// vpath patterns.
    /// These are vpaths that only apply to files matching particular patterns
    pub patterned_vpaths: pattern::PatternEngine<Vec<PathBuf>>,
}

impl Engine {
    /// Gets the actual path for a file, given a file name
    /// This is used for both prereq and target name searches
    /// See [the GNU make manual](https://www.gnu.org/software/make/manual/make.html#Directory-Search)
    /// for details
    pub fn get_path_for_filename(&self, _filename: &str) -> PathBuf {
        // match against unconditional vpaths, and then the pattern engine vpaths
        // https://www.gnu.org/software/make/manual/make.html#Search-Algorithm
        unimplemented!("get_path_for_filename")
    }

    /// Add a rule to the database
    fn add_rule(&mut self, rule: Rule) {
        self.database.add_rule(rule);
    }
}

impl Default for Engine {
    fn default() -> Engine {
        Engine {
            database: Default::default(),
            command_char: '\t',
            vpaths: Default::default(),
            patterned_vpaths: Default::default(),
        }
    }
}

impl Engine {
    /// Parses a makefile
    pub fn read_makefile<F: BufRead>(
        &mut self,
        input: &mut F,
        input_filename: &str,
    ) -> Result<(), MakefileError> {
        let mut parser_state = parsers::ParserState::new(input_filename);

        let mut i = String::new();
        input.read_to_string(&mut i)?;
        let i = &i;
        let mut i = Span::new(nom::types::CompleteStr(i));

        i = match opt!(i, char!('\u{feff}')) {
            Ok((i, _)) => i,
            Err(nom::Err::Failure(_)) => {
                unimplemented!("Something went really wrong when detecting the UTF-8 BOM")
            }
            Err(_) => {
                /* Incomplete or recoverable errors (Error) are fine, that just
                 * means there wasn't a BOM */
                i
            }
        };

        use nom::AtEof;
        use nom::Err as NErr;

        while !i.at_eof() {
            let (new_i, line_action) =
                match parsers::LineParser::new(self, &mut parser_state).parse_line(i) {
                    Ok(v) => v,
                    Err(NErr::Failure(_)) => unimplemented!("Unrecoverable line parsing failure"),
                    Err(NErr::Incomplete(_)) => unimplemented!("incomplete line"),
                    Err(NErr::Error(_)) => unimplemented!("Recoverable line parsing failure"),
                };

            i = new_i;

            eprintln!("{:?}", line_action);
        }

        // for line_data in readline::LineReader::new(input) {
        //     let (line_no, line) = line_data?;

        //     if line.len() == 0 {
        //         // Nothing to do for empty lines
        //         continue;
        //     }

        // // If the first character is the command character, this is a command line.
        // if first_char == self.command_char {
        //     unimplemented!("Parsing command lines");
        // }

        // // We've detected command characters, so we don't care about whitespace anymore.
        // // Strip it
        // let trimmed_line = line.trim();
        // parser_state.line_chr += line.len() - trimmed_line.len();

        // // If the line is now zero length, there is nothing for us to do
        // if trimmed_line.len() == 0 {
        //     continue;
        // }

        // Try treating this as a variable assignment
        // if self.parse_line_as_variable_operation(&trimmed_line, &mut parser_state)? {
        //     continue;
        // }

        // NOTE: the GNU Make implementation keeps track of "in_ignored_define"
        // but never seems to use it (read.c:769)

        // We have now done all the processing that will be done for ignored lines
        // if parser_state.ignoring {
        //     continue;
        // }
        // }

        Ok(())
    }

    /*
    /// Parse a conditional line.
    /// Returns true if the line was processed by this function.
    /// Updates the parse state to reflect conditions
    fn parse_conditional_line(
        &mut self,
        line: &str,
        parser_state: &mut ParserState,
    ) -> Result<bool, ParseError> {
        if line.starts_with("ifdef") {
            unimplemented!()
        } else if line.starts_with("ifndef") {
            unimplemented!()
        } else if line.starts_with("ifeq") {
            unimplemented!()
        } else if line.starts_with("ifneq") {
            unimplemented!()
        } else if line.starts_with("else") {
            if let Some(conditional) = parser_state.conditionals.last_mut() {
                if conditional.seen_else {
                    return Err(ParseError::new(parser_state, ParseErrorKind::TooManyElses));
                }
    
                conditional.seen_else = true;
                // For some reason, on read.c:1622, the GNU make implementation of this function
                // sets "ignoring" to 2 (via ignoring[o] = 2). I don't know why they have 3 states
                conditional.interpretation = match conditional.interpretation {
                    // Conditionals that just execute should definitely ignore in their else branch
                    ConditionalInterpretation::AllowExecute => {
                        ConditionalInterpretation::DefinitelyIgnore
                    }
                    // Conditionals that didn't execute before are allowed to now
                    ConditionalInterpretation::DefinitelyIgnore => {
                        ConditionalInterpretation::AllowExecute
                    }
                }
            } else {
                return Err(ParseError::new(
                    parser_state,
                    ParseErrorKind::MismatchedBranchCommand,
                ));
            }
        } else if line.starts_with("endif") {
            if line.len() != "endif".len() {
                return Err(ParseError::new(
                    parser_state,
                    ParseErrorKind::ExtraTokensAfter("endif"),
                ));
            }
    
            if parser_state.conditionals.len() == 0 {
                return Err(ParseError::new(
                    parser_state,
                    ParseErrorKind::MismatchedBranchCommand,
                ));
            }
    
            parser_state.conditionals.pop();
        } else {
            return Ok(false);
        }
    
        for conditional in &parser_state.conditionals {
            if conditional.interpretation == ConditionalInterpretation::DefinitelyIgnore {
                parser_state.ignoring = true;
                return Ok(true);
            }
        }
        parser_state.ignoring = false;
    
        return Ok(true);
    }
     */
}

/*
#[cfg(test)]
mod test {
    use crate::ParserState;

    fn clean_parser_state<'a>(fname: &'a str) -> ParserState<'a> {
        ParserState::new(fname)
    }

    mod parse_conditional {
        use super::clean_parser_state;
        use crate::{Engine, ParseError, ParseErrorKind};

        #[test]
        fn test_endif_extra_chars() {
            let mut engine: Engine = Default::default();
            let mut parse_state = clean_parser_state("test.mk");
            let parse_state_reference = parse_state.clone();

            assert_eq!(
                engine.parse_conditional_line("endif asdf", &mut parse_state),
                Err(ParseError::new_test(
                    1,
                    1,
                    ParseErrorKind::ExtraTokensAfter("endif")
                ))
            );

            assert_eq!(parse_state, parse_state_reference);
        }

        #[test]
        fn test_endif_bad_depth() {
            let mut engine: Engine = Default::default();
            let mut parse_state = clean_parser_state("test.mk");
            let parse_state_reference = parse_state.clone();

            assert_eq!(
                engine.parse_conditional_line("endif", &mut parse_state),
                Err(ParseError::new_test(
                    1,
                    1,
                    ParseErrorKind::MismatchedBranchCommand
                ))
            );

            assert_eq!(parse_state, parse_state_reference);
        }
    }
}
*/

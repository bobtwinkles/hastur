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

// #[cfg(test)]
// #[macro_use]
// extern crate lazy_static;

/// Internal macros for use in test assertions
#[cfg(test)]
#[macro_use]
mod test_macros;

pub mod ast;
mod eval;
pub mod evaluated;
mod parsers;
pub mod pattern;
pub mod source_location;
pub mod tokenizer;
pub mod traits;
mod types;

pub use crate::eval::{Flavor, Origin, Variable, VariableParameters};
pub use crate::parsers::ParserCompliance;

use crate::ast::AstNode;
use crate::evaluated::Block;
use crate::source_location::{LocatedString, Location};
use std::io;
use std::io::prelude::*;
use std::path::PathBuf;
use std::sync::Arc;
use string_interner::DefaultStringInterner as StringInterner;
use string_interner::Sym;

/// What sort of thing went wrong while parsing.
#[derive(Clone, Debug, PartialEq)]
pub enum ParseErrorKind {
    /// Generated when there are tokens after something we expect to be the end of a line
    ExtraTokensAfter(&'static str),

    /// A free-floating else was encountered
    UnattachedElse,

    /// A free-floating endif was encountered
    UnattachedEndIf,

    /// Trying to write something like
    /// ```makefile
    /// if
    /// else
    /// else
    /// ```
    TooManyElses,

    /// We expected to find a comment (something starting with #) but found something else
    CommentExpected,

    /// A recipe started without a corresponding rule
    NoRuleRecipe,

    /// There was a line that didn't match any sort of command or variable assignment, and also wasn't a rule
    MissingSeparator,

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

    /// A variable reference of the form `$()` or `${}` was not terminated
    /// before the end of the line
    UnternimatedVariable,

    /// A function was invoked with too few arguments. The returned string
    /// indicates the function name
    InsufficientArguments(&'static str),

    /// A function was invoked with too many arguments.
    ExtraArguments(&'static str),

    /// Failed to include a file
    /// TODO: this should really be an std::io::Error instead of an ErrorKind,
    /// but writing the PartialEq/Clone implementations by hand for this enum
    /// is a pain
    IncludeFailure(std::io::ErrorKind, String),

    /// This variant should never surface in library consumer code in practice,
    /// but we need it to make nom types work out
    NomError(u32),

    /// Another internal variant. Should never be returned to the library consumer
    InternalFailure(&'static str),
}

impl<'a> From<u32> for ParseErrorKind {
    fn from(other: u32) -> ParseErrorKind {
        ParseErrorKind::NomError(other)
    }
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
    unexpanded_command: AstNode,
}

/// Represents what type of rule this is (standard, double colon, static pattern, etc.)
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum RuleType {
    /// A plain-jane `a: b c` type of rule
    Standard,
    /// This is a double colon rule
    DoubleColon,
}

/// The type of a recipe
#[derive(Clone, Debug, PartialEq)]
pub struct Recipe(Vec<Command>);

impl Recipe {
    /// Get the list of commands backing this recipe
    pub fn commands(&self) -> &[Command] {
        &self.0
    }
}

/// A "rule" describes how to bring a set of "targets" up to date from a set of
/// "dependencies", using a "recipe".
#[derive(Clone, Debug, PartialEq)]
pub struct Rule {
    /// The target that this rule builds
    target: Arc<Block>,
    /// The target, as an interned string
    target_fname: FileName,
    /// The inputs for the recipe
    deps: Vec<Arc<Block>>,
    /// File names, cached as interned strings
    dep_names: fxhash::FxHashSet<FileName>,
    /// The recipe to turn `deps` into `targets`
    recipe: Arc<Recipe>,
    /// Extra information about the rule
    rule_type: RuleType,
}

// TODO: move this impl into rule.rs
impl Rule {
    /// Get a reference to the block that defined the name of this target
    pub fn target(&self) -> &Arc<Block> {
        &self.target
    }

    /// Get the target as an interned file name
    pub fn target_fname(&self) -> FileName {
        self.target_fname
    }

    /// Get a reference to the recipe behind this command
    pub fn recipe(&self) -> &Arc<Recipe> {
        &self.recipe
    }

    /// Iterate over the dependencies of this rule
    pub fn dependencies(&self) -> &[Arc<Block>] {
        &self.deps
    }
}

/// Type representing an opaque variable name reference.
/// It is a logical bug to share these between database instances.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct VariableName(Sym);

/// Type representing an opaque file name reference.
/// It is a logical bug to share these between database instances.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct FileName(Sym);

/// Database of rules and variables.
/// This represents the full context required to evaluate something in Makefile syntax
#[derive(Clone, Default)]
#[must_use]
pub struct Database {
    /// Global variables
    variables: types::Map<VariableName, VariableParameters>,
    /// Target variables, mapped from target name to variable name to value
    target_variables: types::Map<FileName, types::Map<VariableName, VariableParameters>>,
    /// All known rules
    rules: types::Map<FileName, Rule>,
}

impl Database {
    /// Iterate over all the rules in this database
    pub fn rules(&self) -> impl Iterator<Item = &Rule> {
        self.rules.values()
    }

    /// Get the rules for a given target
    pub fn rule_for_target(&self, target: FileName) -> Option<&Rule> {
        self.rules.get(&target)
    }

    /// Add a rule into the database
    pub fn add_rule(&self, rule: Rule) -> Self {
        let mut tr = self.clone();
        tr.rules.insert(rule.target_fname, rule);
        tr
    }

    /// Create a rule from the a protorule
    fn from_protorule(&self, names: &mut NameCache, rule: crate::parsers::ProtoRule) -> Self {
        let mut tr = self.clone();
        let has_no_commands = rule.recipe.0.len() == 0;
        let recipe = Arc::new(rule.recipe);
        let deps = &rule.deps;
        debug!("Intern dep names {:?}", deps);
        let deps_as_fnames: fxhash::FxHashSet<FileName> = deps
            .iter()
            .map(|dep| names.intern_file_name(dep.into_string()))
            .collect();
        let deps_as_fnames = &deps_as_fnames;
        let rule_type = rule.rule_type;

        // Empty recipes are special: these rules may append to the dep list, while
        // other rules may not (and instead must overwrite the previous rule)
        if has_no_commands {
            debug!("protorule for targets {:?} had no commands", rule.targets);
            for target in rule.targets.into_iter() {
                let target_as_fname = names.intern_file_name(target.into_string());
                tr.rules
                    .entry(target_as_fname)
                    .and_modify(|existing_rule| {
                        for dep in deps {
                            existing_rule.deps.push(Arc::clone(dep));
                        }
                        for dep in deps_as_fnames {
                            existing_rule.dep_names.insert(*dep);
                        }
                    })
                    .or_insert_with(|| Rule {
                        target,
                        target_fname: target_as_fname,
                        deps: deps.iter().map(|x| Arc::clone(x)).collect(),
                        dep_names: deps_as_fnames.clone(),
                        recipe: Arc::clone(&recipe),
                        rule_type,
                    });
            }
        } else {
            use im::hashmap::Entry;
            debug!(
                "protorule for targets {:?} had commands {:?}",
                rule.targets, recipe
            );

            for target in rule.targets.into_iter() {
                let target_as_fname = names.intern_file_name(target.into_string());
                let new_rule = Rule {
                    target,
                    target_fname: target_as_fname,
                    deps: rule.deps.iter().map(|x| Arc::clone(x)).collect(),
                    dep_names: deps_as_fnames.clone(),
                    recipe: Arc::clone(&recipe),
                    rule_type: rule.rule_type,
                };
                match tr.rules.entry(target_as_fname) {
                    Entry::Occupied(mut o) => {
                        // TODO: bubble this up in a structured manner instead of just printing nonsense
                        warn!("Overwriting existing rule for target {:?}", target_as_fname);
                        o.insert(new_rule);
                    }
                    Entry::Vacant(v) => {
                        v.insert(new_rule);
                    }
                }
            }
        }

        tr
    }

    /// Set the value of a variable
    #[inline]
    pub fn set_variable(&self, name: VariableName, value: VariableParameters) -> Self {
        info!("Setting variable {:?} to {:?}", name, value);
        let mut tr = self.clone();
        tr.variables.insert(name, value);
        tr
    }

    /// Append to a variable at the specified location
    /// The provided parameters are all used to update the variable state,
    /// though if the variable was previously defined.
    /// If `implicit_space` is true, we add a space between any old and new values.
    #[inline]
    pub fn append_to_variable(
        &self,
        name: VariableName,
        location: Location,
        content: VariableParameters,
        implicit_space: bool,
    ) -> Self {
        let mut tr = self.clone();
        // tr.variables.
        tr.variables = tr.variables.update_with(name, content, |mut old, new| {
            old.unexpanded_value = ast::collapsing_concat(
                location,
                if implicit_space {
                    vec![
                        old.unexpanded_value,
                        ast::constant(LocatedString::new(Location::Synthetic.into(), " ".into())),
                        new.unexpanded_value,
                    ]
                } else {
                    vec![old.unexpanded_value, new.unexpanded_value]
                },
            );

            old
        });

        tr
    }

    /// Override the value of a variable for a specific target
    pub fn set_variable_for_target(
        &self,
        target: FileName,
        name: VariableName,
        value: VariableParameters,
    ) -> Self {
        let mut tr = self.clone();
        let target = target.into();
        tr.target_variables
            .entry(target)
            .or_default()
            .insert(name, value);
        tr
    }

    /// Get a variable based on a name
    pub fn get_variable(&self, name: VariableName) -> Option<Variable> {
        self.variables
            .get(&name)
            .map(|val| Variable::new(self, val))
    }

    /// Get a variable based on the target and name
    pub fn get_variable_for_target(
        &self,
        target: FileName,
        name: VariableName,
    ) -> Option<Variable> {
        self.target_variables
            .get(&target)?
            .get(&name)
            .map(|val| Variable::new(self, val))
    }

    /// Get a rule for a specific target
    pub fn get_rule(&self, target: FileName) -> Option<&Rule> {
        self.rules.get(&target)
    }
}

impl std::fmt::Debug for Database {
    /// Global variables
    // variables: types::Map<VariableName, VariableParameters>,
    /// Target variables, mapped from target name to variable name to value
    // target_variables: types::Map<FileName, types::Map<VariableName, VariableParameters>>,
    /// All known rules
    // rules: types::Map<FileName, Rule>,
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Database {{\n")?;

        write!(f, "  variables:\n")?;
        write!(f, "  {:?}\n", self.variables)?;

        write!(f, "  target_variables:\n")?;
        write!(f, "  {:?}\n", self.target_variables)?;

        write!(f, "  rules:\n")?;
        write!(f, "  {:?}\n", self.rules)?;

        write!(f, "}}")
    }
}

/// Represents all the things that can go wrong while parsing an evaluating
/// makefile statements.
#[derive(Debug)]
pub enum MakefileError {
    /// An IO error occurred while reading the makefile
    IOError(io::Error),
    /// The parser wasn't able to handle some of the syntax
    ParseError(ParseErrorKind),
}

impl From<io::Error> for MakefileError {
    fn from(other: io::Error) -> Self {
        MakefileError::IOError(other)
    }
}

/*
impl<'a> From<ParseError<'a>> for MakefileError {
    fn from(other: ParseError<'a>) -> Self {
        MakefileError::ParseError(other.to_static())
    }
}
*/

/// Cache of all the different kinds of name we can encounter in a makefile.
/// There should probably only be one of these, so this intentionally doesn't
/// implement `Clone`
#[derive(Default, Debug, Clone)]
pub struct NameCache {
    /// All the file names
    file_names: StringInterner,
    /// All known variable names
    variable_names: StringInterner,
}

impl NameCache {
    /// Intern a the file name for a makefile
    pub fn intern_file_name(&mut self, file_name: String) -> FileName {
        let trid = self.file_names.get_or_intern(file_name);
        info!(
            "Intern filename {:?} as {:?}",
            self.file_names.resolve(trid),
            trid
        );
        FileName(trid)
    }

    /// Try to get a file name that has been interned
    pub fn file_name(&self, file_name: &str) -> Option<FileName> {
        self.file_names.get(file_name).map(FileName)
    }

    /// Get a file name
    pub fn resolve_file_name(&self, f: FileName) -> Option<&str> {
        self.file_names.resolve(f.0)
    }

    /// Intern a variable name
    pub fn intern_variable_name(&mut self, variable_name: String) -> VariableName {
        info!("Intern variable {:?}", variable_name);
        VariableName(self.variable_names.get_or_intern(variable_name))
    }

    /// Try to get a variable name from a string
    pub fn variable_name(&self, variable_name: &str) -> Option<VariableName> {
        self.variable_names.get(variable_name).map(VariableName)
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

    /// The current working directory
    pub working_directory: PathBuf,
}

impl Default for Engine {
    fn default() -> Engine {
        Engine {
            database: Default::default(),
            command_char: '\t',
            vpaths: Default::default(),
            patterned_vpaths: Default::default(),
            working_directory: std::env::current_dir()
                .expect("Failed to get current working directory"),
        }
    }
}

impl Engine {
    // Gets the actual path for a file, given a file name
    /// This is used for both prereq and target name searches
    /// See [the GNU make manual](https://www.gnu.org/software/make/manual/make.html#Directory-Search)
    /// for details
    pub fn get_path_for_filename(&self, _filename: &str) -> PathBuf {
        // match against unconditional vpaths, and then the pattern engine vpaths
        // https://www.gnu.org/software/make/manual/make.html#Search-Algorithm
        unimplemented!("get_path_for_filename")
    }

    /// Update the internal database.
    pub fn replace_database(&mut self, db: Database) {
        self.database = db;
    }

    /// Add a rule to the database
    fn from_protorule(&mut self, names: &mut NameCache, rule: parsers::ProtoRule) {
        self.database = self.database.from_protorule(names, rule);
    }

    /// Parses a makefile
    pub fn read_makefile<F: BufRead>(
        &mut self,
        names: &mut NameCache,
        input: &mut F,
        input_filename: &str,
    ) -> Result<(), MakefileError> {
        info!("Begin reading makefile {:?}", input_filename);

        let mut i = String::new();
        input.read_to_string(&mut i)?;
        let input_block = Block::new(
            Default::default(),
            vec![evaluated::ContentReference::new_from_node(Arc::new(
                evaluated::nodes::EvaluatedNode::Constant(source_location::LocatedString::new(
                    source_location::Location::SourceLocation {
                        character: 1,
                        line: 1,
                        filename: names.intern_file_name(input_filename.into()),
                    }
                    .into(),
                    i,
                )),
            ))],
        );

        self.process_block(names, &input_block)?;

        info!("Complete read of makefile {:?}", input_filename);

        Ok(())
    }

    /// Process a block of makefile text
    pub fn process_block(
        &mut self,
        names: &mut NameCache,
        block: &Block,
    ) -> Result<(), MakefileError> {
        use nom::Err as NErr;
        let mut parser_state = parsers::ParserState::new();

        let mut i = block.span();

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

        while i.len() > 0 {
            let (new_i, _) = match parser_state.parse_line(i, names, self) {
                Ok(v) => v,
                Err(NErr::Failure(_)) => unimplemented!("Unrecoverable line parsing failure"),
                Err(NErr::Incomplete(_)) => unimplemented!("incomplete line"),
                Err(NErr::Error(_)) => unimplemented!("Recoverable line parsing failure"),
            };

            i = new_i;
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    //! Test utilities
    pub(crate) fn setup() {
        let _ = env_logger::builder()
            .is_test(true)
            .filter_level(log::LevelFilter::Debug)
            .try_init();
    }

    /*
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
     */
}

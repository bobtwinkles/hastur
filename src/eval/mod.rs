//! Utilities for evaluating makefile expressions

use crate::parsers::CollapsedLineSpan;
use crate::{Database, MakefileError, OwnedFragment};
use string_interner::Sym;

#[cfg(test)]
mod test;

/// What flavor is this variable
enum Flavor {
    /// For variable that can't be expanded for whatever reason
    Bogus,
    /// Simple (non-recursively expanded, not from shell, etc.) variables
    /// Created from := or ::= statements
    Simple,
    /// Recursively expanded variable (assigned via =)
    Recursive,
    /// An appending definition from +=
    Append,
    /// A definition from a ?= statement
    Conditional,
    /// A shell assignment (!=)
    Shell,
    /// Append unexpanded value
    AppendUnexpanded,
}

/// Where did the variable come from
pub enum Origin {
    /// This variable was set by Make itself in the default set
    Default,
    /// The variable came from the environment
    Environment,
    /// The variable was set by a file that we read in
    File,
    /// The variable was set by an environment override (-e flag in GNU Make)
    EnvironmentOverride,
    /// The user issued a command to set this variable
    Command,
    /// An automatically generated variable
    Automatic,
    /// Make says that these are "core dump time", may not actually be needed in Hastur
    Invalid,
}

/// Everything we need to know to expand a variable
pub struct VariableParameters {
    unexpanded_value: OwnedFragment<Sym>,
    flavor: Flavor,
    origin: Origin,
}

impl VariableParameters {
    fn new(unexpanded_value: OwnedFragment<Sym>, flavor: Flavor, origin: Origin) -> Self {
        Self {
            unexpanded_value,
            flavor,
            origin,
        }
    }
}

/// Represents a view into a make variable.
/// These provide some convenience functions for expansion, but don't keep them
/// around since they tie up exclusive access to the database.
pub struct Variable<'d> {
    value: &'d VariableParameters,
    database: &'d Database,
    target: Option<&'d str>,
}

impl<'d> Variable<'d> {
    /// Create a new variable with a given value
    pub fn new(db: &'d Database, value: &'d VariableParameters) -> Self {
        Self {
            value,
            database: db,
            target: None,
        }
    }

    /// Create a new variable tied to a specific target, with a given value
    pub fn new_for_target(db: &'d Database, value: &'d VariableParameters, fname: &'d str) -> Self {
        Self {
            value,
            database: db,
            target: Some(fname),
        }
    }

    /// Expand this variable in the environment provided by a database
    pub fn expand(&self, environment: &Database) -> String {
        unimplemented!("variable expansion")
    }
}

pub(crate) fn expand_line<'a, 'line: 'a>(
    database: &Database,
    mut line: CollapsedLineSpan<'a, 'line>,
) -> Result<String, MakefileError> {
    let mut output = String::with_capacity(line.len());

    // while line.len() > {
    //     let (new_line, expanded_content) = match 
    // }

    Ok(output)
}

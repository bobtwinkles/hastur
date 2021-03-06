//! Utilities for evaluating makefile expressions

use crate::ast::AstNode;
use crate::{Database, Engine, NameCache};

#[cfg(test)]
mod test;

/// What flavor is this variable
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Flavor {
    /// Simple (non-recursively expanded, not from shell, etc.) variables
    /// Created from := or ::= statements
    Simple,
    /// Recursively expanded variable (assigned via =)
    Recursive,
    /// An appending definition from +=
    Conditional,
    /// A shell assignment (!=)
    Shell,
}

/// Where did the variable come from
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
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
#[derive(Clone, Debug, PartialEq)]
pub struct VariableParameters {
    pub(crate) unexpanded_value: AstNode,
    pub(crate) flavor: Flavor,
    pub(crate) origin: Origin,
}

impl VariableParameters {
    pub(crate) fn new(unexpanded_value: AstNode, flavor: Flavor, origin: Origin) -> Self {
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
#[derive(Copy, Clone, Debug)]
pub struct Variable<'d> {
    pub(crate) value: &'d VariableParameters,
    _database: &'d Database,
    target: Option<&'d str>,
}

impl<'d> Variable<'d> {
    /// Create a new variable with a given value
    pub fn new(db: &'d Database, value: &'d VariableParameters) -> Self {
        Self {
            value,
            _database: db,
            target: None,
        }
    }

    /// Create a new variable tied to a specific target, with a given value
    pub fn new_for_target(db: &'d Database, value: &'d VariableParameters, fname: &'d str) -> Self {
        Self {
            value,
            _database: db,
            target: Some(fname),
        }
    }

    /// Expand this variable in the environment provided by a database.
    /// Shorthand for [`self.ast().eval(environment).into_string()`](struct.Variable.html#method.ast)
    pub fn expand(&self, names: &mut NameCache, environment: &mut Engine) -> (String) {
        match self.target {
            Some(_) => unimplemented!("Expansion in target context"),
            None => {
                let content = self.ast().eval(names, environment);
                content.into_string()
            }
        }
    }

    /// Get the AST of this node
    pub fn ast(&self) -> &AstNode {
        &self.value.unexpanded_value
    }
}

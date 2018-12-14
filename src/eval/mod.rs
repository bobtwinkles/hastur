//! Utilities for evaluating makefile expressions

use crate::{Database, OwnedFragment};

#[cfg(test)]
mod test;

/// Represents a view into a make variable.
/// These provide some convenience functions for expansion, but don't keep them
/// around since they tie up exclusive access to the database.
pub struct Variable<'d> {
    unexpanded_value: &'d OwnedFragment<&'d str>,
    database: &'d Database,
    target: Option<&'d str>,
}

impl<'d> Variable<'d> {
    /// Create a new variable with a given value
    pub fn new(db: &'d Database, value: &'d OwnedFragment<&'d str>) -> Self {
        Self {
            unexpanded_value: value,
            database: db,
            target: None,
        }
    }

    /// Create a new variable tied to a specific target, with a given value
    pub fn new_for_target(
        db: &'d Database,
        value: &'d OwnedFragment<&'d str>,
        fname: &'d str,
    ) -> Self {
        Self {
            unexpanded_value: value,
            database: db,
            target: Some(fname),
        }
    }

    /// Expand this variable in the environment provided by a database
    pub fn expand(&self, environment: &Database) -> String {
        unimplemented!("variable expansion")
    }
}

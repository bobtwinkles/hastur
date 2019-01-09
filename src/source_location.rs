//! All the types required to interact with source locations
// #SPC-Location

use crate::trees::evaluated::EvaluatedTree;
use crate::Sym;
use std::rc::Rc;

/// An internal structure representing a source location.
/// This is intentionally kept separate from the source location
/// exposed publicly in order to facilitate switching to an interning scheme
/// later.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct Marker {
    pub(crate) inner: Location,
}

/// The public location type.
// #REQ-Location
#[derive(Clone, Debug, Eq)]
pub enum Location {
    /// Represents a location in a given source file
    // #REQ-Location.raw_location
    // #SPC-Location.raw_location
    SourceLocation {
        line: u32,
        character: u32,
        filename: Sym,
    },
    /// Represents a location in source resulting from an `$(eval ...)` block
    // #REQ-Location.eval_location
    // #SPC-Location.eval_location
    EvalLocation {
        tree_root: Rc<EvaluatedTree>,
        line: u32,
        character: u32,
    },
}

// #REQ-Location.comparable
impl std::cmp::PartialEq for Location {
    fn eq(&self, o: &Location) -> bool {
        match (self, o) {
            (
                Location::SourceLocation {
                    line: a_line,
                    character: a_character,
                    filename: a_fname,
                },
                Location::SourceLocation {
                    line: b_line,
                    character: b_character,
                    filename: b_fname,
                },
            ) => a_fname == b_fname && a_line == b_line && a_character == b_character,
            (
                Location::EvalLocation {
                    tree_root: a_root,
                    line: a_line,
                    character: a_character,
                },
                Location::EvalLocation {
                    tree_root: b_root,
                    line: b_line,
                    character: b_character,
                },
            ) => {
                // Even if two blocks evaluated to the same thing,
                // we only want to compare locations within the output from the same evaluation.
                Rc::ptr_eq(a_root, b_root) && a_line == b_line && a_character == b_character
            }
            _ => false,
        }
    }
}

impl std::cmp::PartialOrd for Location {
    fn partial_cmp(&self, o: &Location) -> Option<std::cmp::Ordering> {
        let (a_line, b_line, a_character, b_character) = match (self, o) {
            (
                Location::SourceLocation {
                    line: a_line,
                    character: a_character,
                    filename: a_fname,
                },
                Location::SourceLocation {
                    line: b_line,
                    character: b_character,
                    filename: b_fname,
                },
            ) => {
                if a_fname != b_fname {
                    None
                } else {
                    Some((a_line, b_line, a_character, b_character))
                }
            }
            (
                Location::EvalLocation {
                    tree_root: a_root,
                    line: a_line,
                    character: a_character,
                },
                Location::EvalLocation {
                    tree_root: b_root,
                    line: b_line,
                    character: b_character,
                },
            ) => {
                // Even if two blocks evaluated to the same thing,
                // we only want to compare locations within the output from the same evaluation.
                if Rc::ptr_eq(a_root, b_root) {
                    Some((a_line, b_line, a_character, b_character))
                } else {
                    None
                }
            }
            _ => {
                // There is no reasonable way to order comparisons across different source location types
                None
            }
        }?;
        if a_line == b_line {
            a_character.partial_cmp(b_character)
        } else {
            a_line.partial_cmp(b_line)
        }
    }
}

impl Location {
    /// Move to the next line, resetting the character counter
    pub(crate) fn advance_line(&self) -> Location {
        match self {
            Location::EvalLocation {
                tree_root, line, ..
            } => Location::EvalLocation {
                tree_root: tree_root.clone(),
                line: line + 1,
                character: 1,
            },
            Location::SourceLocation { filename, line, .. } => Location::SourceLocation {
                filename: *filename,
                line: line + 1,
                character: 1,
            },
        }
    }

    /// Move to the next character within a line
    pub(crate) fn advance_character(&self, amount: u32) -> Location {
        match self {
            Location::EvalLocation {
                tree_root,
                line,
                character,
            } => Location::EvalLocation {
                tree_root: tree_root.clone(),
                line: *line,
                character: character + amount,
            },
            Location::SourceLocation {
                filename,
                line,
                character,
            } => Location::SourceLocation {
                filename: *filename,
                line: *line,
                character: character + amount,
            },
        }
    }
}

impl Location {}

/// Wraps an arbitrary thing (probably a string slice or some wrapper around a
/// string slice) that has a location.
#[derive(Clone, Debug, PartialEq)]
pub struct Located<T> {
    location: Marker,
    contents: T,
}

impl<T> std::ops::Deref for Located<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.contents
    }
}

impl<T> Located<T> {
    /// Get the location of this thing
    pub fn location(&self) -> &Location {
        &self.location.inner
    }
}

/// A string span
pub type StringSpan<'a> = Located<&'a str>;
/// A string span that is owned
pub type OwnedStringSpan = Located<String>;

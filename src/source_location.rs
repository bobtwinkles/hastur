//! All the types required to interact with source locations
// #SPC-Location
use crate::evaluated::Block;
use crate::FileName;
use std::sync::Arc;

#[cfg(test)]
pub(crate) mod test {
    use super::*;

    /// Test utility to create a located span
    pub(crate) fn span(content: &str) -> LocatedString {
        Located::new(
            Location::TestLocation {
                line: 1,
                character: 1,
            }
            .into(),
            content.into(),
        )
    }

    /// Test utility to create a located span
    pub(crate) fn span_with_location(content: &str, line: u32, character: u32) -> LocatedString {
        Located::new(
            Location::TestLocation { line, character }.into(),
            content.into(),
        )
    }
}

/// An internal structure representing a source location.
/// This is intentionally kept separate from the source location
/// exposed publicly in order to facilitate switching to an interning scheme
/// later.
#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Marker {
    pub(crate) inner: Location,
}

impl From<Location> for Marker {
    fn from(loc: Location) -> Marker {
        Marker { inner: loc }
    }
}

/// The public location type.
// #REQ-Location
// #REQ-Location.comparable
#[derive(Clone, Debug, PartialEq)]
pub enum Location {
    /// Represents a location in a given source file
    // #REQ-Location.raw_location
    // #SPC-Location.raw_location
    SourceLocation {
        /// The line
        line: u32,
        /// The character in the line
        character: u32,
        /// The filename of the file this comes from.
        filename: FileName,
    },
    /// Represents a location in source resulting from an `$(eval ...)` block
    // #REQ-Location.eval_location
    // #SPC-Location.eval_location
    EvalLocation {
        /// The node in the evaluation tree this points to
        tree_node: Arc<Block>,
        /// The line
        line: u32,
        /// The character
        character: u32,
    },
    /// Synthetically generated text from a Make invocation
    Synthetic,
    /// A location from inside a test string
    #[cfg(test)]
    TestLocation { line: u32, character: u32 },
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
                    tree_node: a_node,
                    line: a_line,
                    character: a_character,
                },
                Location::EvalLocation {
                    tree_node: b_node,
                    line: b_line,
                    character: b_character,
                },
            ) => {
                // Even if two blocks evaluated to the same thing,
                // we only want to compare locations within the output from the same evaluation.
                if a_node == b_node {
                    Some((a_line, b_line, a_character, b_character))
                } else {
                    None
                }
            }
            (Location::Synthetic, Location::Synthetic) => return Some(std::cmp::Ordering::Equal),
            #[cfg(test)]
            (
                Location::TestLocation {
                    line: a_line,
                    character: a_character,
                },
                Location::TestLocation {
                    line: b_line,
                    character: b_character,
                },
            ) => Some((a_line, b_line, a_character, b_character)),
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
    /// Shorthand for creating a location for use in tests
    #[cfg(test)]
    pub fn test_location(line: u32, character: u32) -> Location {
        Location::TestLocation { line, character }
    }

    /// Move to the next line, resetting the character counter
    #[inline]
    pub(crate) fn advance_line(&self) -> Location {
        match self {
            Location::EvalLocation {
                tree_node, line, ..
            } => Location::EvalLocation {
                tree_node: tree_node.clone(),
                line: line + 1,
                character: 1,
            },
            Location::SourceLocation { filename, line, .. } => Location::SourceLocation {
                filename: *filename,
                line: line + 1,
                character: 1,
            },
            Location::Synthetic => Location::Synthetic,
            #[cfg(test)]
            Location::TestLocation { line, .. } => Location::TestLocation {
                line: line + 1,
                character: 1,
            },
        }
    }

    /// Move to the next character within a line
    #[inline]
    pub(crate) fn advance_character(&self) -> Location {
        match self {
            Location::EvalLocation {
                tree_node,
                line,
                character,
            } => Location::EvalLocation {
                tree_node: tree_node.clone(),
                line: *line,
                character: character + 1,
            },
            Location::SourceLocation {
                filename,
                line,
                character,
            } => Location::SourceLocation {
                filename: *filename,
                line: *line,
                character: character + 1,
            },
            Location::Synthetic => Location::Synthetic,
            #[cfg(test)]
            Location::TestLocation { line, character } => Location::TestLocation {
                line: *line,
                character: character + 1,
            },
        }
    }
}

impl Location {}

/// Wraps an arbitrary thing (probably a string slice or some wrapper around a
/// string slice) that has a location.
#[derive(Clone, Debug, PartialEq)]
pub struct Located<T> {
    pub(crate) location: Marker,
    pub(crate) contents: T,
}

impl<T: PartialEq> PartialOrd for Located<T> {
    fn partial_cmp(&self, o: &Located<T>) -> Option<std::cmp::Ordering> {
        self.location.inner.partial_cmp(&o.location.inner)
    }
}

impl<T> std::ops::Deref for Located<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.contents
    }
}

impl<T> Located<T> {
    /// Create a new located T
    pub(crate) fn new(location: Marker, contents: T) -> Self {
        Self { location, contents }
    }

    /// Get the location of this thing
    pub fn location(&self) -> &Location {
        &self.location.inner
    }
}

impl<'a> Located<&'a str> {
    /// Get a subslice of this slice
    pub fn slice(&self, bytes: usize, length: usize) -> LocatedStr<'a> {
        assert!(bytes < self.contents.len());
        assert!(bytes + length <= self.contents.len());
        let mut new_location = self.location.inner.clone();
        for c in self.contents[0..bytes].chars() {
            if c == '\n' {
                new_location = new_location.advance_line();
            } else {
                new_location = new_location.advance_character();
            }
        }

        Self {
            location: Marker {
                inner: new_location,
            },
            contents: &self.contents[bytes..(bytes + length)],
        }
    }
}

impl Located<String> {
    /// Create a new located string for use in tests
    #[cfg(test)]
    pub fn test_new(line: u32, character: u32, contents: impl Into<String>) -> LocatedString {
        LocatedString::new(
            Location::test_location(line, character).into(),
            contents.into(),
        )
    }

    /// Create a new located string with a synthetic location
    pub fn synthetic_new(contents: impl Into<String>) -> LocatedString {
        LocatedString::new(Location::Synthetic.into(), contents.into())
    }

    /// Slice this string into another owned string. Both the start and length
    /// are in bytes.
    pub fn slice(&self, bytes: usize, length: usize) -> LocatedString {
        self.as_str().slice(bytes, length).into()
    }

    /// Get this string as a located string
    pub fn as_str(&self) -> LocatedStr {
        LocatedStr::new(self.location.clone(), &self.contents)
    }
}

/// A string span
pub type LocatedStr<'a> = Located<&'a str>;
/// A string span that is owned
pub type LocatedString = Located<String>;

impl<'a> From<LocatedStr<'a>> for LocatedString {
    fn from(other: LocatedStr<'a>) -> LocatedString {
        Located {
            location: other.location,
            contents: other.contents.into(),
        }
    }
}

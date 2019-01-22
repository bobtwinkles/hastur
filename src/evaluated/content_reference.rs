//! Reference to makefile content
use crate::evaluated::{Block, EvaluatedNode};
use std::sync::Arc;

/// Represents a reference to some subsection of an evaluated node
#[derive(Clone, Debug, PartialEq)]
pub struct ContentReference {
    /// The offset of this reference into the node (in bytes)
    pub(super) offset: usize,
    /// How much of the referenced node this reference cares about (in bytes)
    pub(super) length: usize,
    /// The backing node
    pub(super) node: Arc<EvaluatedNode>,
}

impl ContentReference {
    /// Create a new content reference that is just everything in a child node
    pub fn new_from_node(node: Arc<EvaluatedNode>) -> Self {
        ContentReference {
            offset: 0,
            length: node.len(),
            node,
        }
    }

    /// Create a new content reference from a slice of a node
    pub fn new_from_node_slice(node: Arc<EvaluatedNode>, offset: usize, length: usize) -> Self {
        assert!(offset < node.len());
        assert!(offset + length <= node.len());

        ContentReference {
            offset,
            length,
            node,
        }
    }

    /// Create a content reference for a synthetically generated space
    pub fn space() -> Self {
        ContentReference {
            offset: 0,
            length: 1,
            node: EvaluatedNode::space(),
        }
    }

    /// Create a content reference for a synthetically generated new line
    pub fn newline() -> Self {
        ContentReference {
            offset: 0,
            length: 1,
            node: EvaluatedNode::newline(),
        }
    }

    /// Append the contents of this reference to a string
    pub fn append_to_string(&self, buffer: &mut String) {
        buffer.reserve(self.length);
        for c in self.chars() {
            buffer.push(c)
        }
    }

    /// Iterate over the characters in this content reference
    pub fn chars(&self) -> ContentReferenceIter {
        ContentReferenceIter(self.node.chars().skip(self.offset).take(self.length))
    }

    /// Iterate over the character indices in this content reference
    pub fn char_indices(&self) -> ContentReferenceIndexIter {
        let iter = self.chars();
        ContentReferenceIndexIter {
            front_offset: 0,
            indices: iter,
        }
    }

    /// Iterate over the segments of this reference
    pub fn segments(&self) -> SegmentsIter {
        SegmentsIter {
            internal: self.node.segments(),
            length: self.length,
            offset: self.offset,
        }
    }

    /// Simplify the node we're pointing to by dropping unnecessary components
    /// This means we rewrite constant nodes completely, realize offsets into concatenations,
    /// and recurse into concatenations
    pub(super) fn simplify(&mut self) {
        use std::borrow::Borrow;
        let new_node = match self.node.borrow() {
            EvaluatedNode::Constant(s) => {
                let tr = Arc::new(EvaluatedNode::Constant(s.slice(self.offset, self.length)));
                self.offset = 0;
                tr
            }
            EvaluatedNode::Concat(ref block) => {
                let mut tr_block = Arc::clone(block);
                let block: &mut Block = Arc::make_mut(&mut tr_block);

                let mut remove_count = 0;
                while block.content[remove_count].length <= self.offset {
                    self.offset -= block.content[remove_count].length;
                    remove_count += 1;
                }
                block.content[remove_count].offset += self.offset;
                block.content[remove_count].length -= self.offset;
                self.offset = 0;
                let mut last_index = remove_count;
                let mut running_length = 0;
                while running_length < self.length {
                    running_length += block.content[last_index].length;
                    last_index += 1;
                }
                block.content[last_index - 1].length -= running_length - self.length;
                block.content = block.content[remove_count..last_index].to_vec();
                block.simplify();

                Arc::new(EvaluatedNode::Concat(tr_block))
            }
            // All other node types are passed through unchanged
            _ => return,
        };
        self.node = new_node;
    }
}

#[derive(Clone, Debug)]
pub struct ContentReferenceIter<'a>(std::iter::Take<std::iter::Skip<super::nodes::Chars<'a>>>);

impl<'a> Iterator for ContentReferenceIter<'a> {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        self.0.next()
    }
}

#[derive(Clone, Debug)]
pub struct ContentReferenceIndexIter<'a> {
    front_offset: usize,
    indices: ContentReferenceIter<'a>,
}

impl<'a> Iterator for ContentReferenceIndexIter<'a> {
    type Item = (usize, char);

    fn next(&mut self) -> Option<(usize, char)> {
        let tr_char = self.indices.next()?;
        let tr_offset = self.front_offset;
        self.front_offset += tr_char.len_utf8();
        Some((tr_offset, tr_char))
    }
}

/// Iterate over chunks of text in this reference
#[derive(Clone, Debug)]
pub struct SegmentsIter<'a> {
    internal: super::nodes::SegmentsIter<'a>,
    offset: usize,
    length: usize,
}

impl<'a> Iterator for SegmentsIter<'a> {
    type Item = crate::source_location::LocatedStr<'a>;

    fn next(&mut self) -> Option<crate::source_location::LocatedStr<'a>> {
        if self.length == 0 {
            return None;
        }

        let mut tr = self.internal.next()?;
        while tr.len() < self.offset {
            self.offset -= tr.len();
            tr = self.internal.next()?;
        }
        tr = tr.slice(self.offset, tr.len() - self.offset);
        self.offset = 0;
        if tr.len() > self.length {
            tr = tr.slice(0, self.length);
        }
        self.length -= tr.len();

        Some(tr)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::evaluated::test::{test_concat_node, test_constant_node};
    use crate::source_location::{LocatedStr, Location};

    #[test]
    fn segments_on_constant() {
        let under_test = ContentReference::new_from_node(test_constant_node("asdf"));

        let mut segments_iter = under_test.segments();
        assert_eq!(
            segments_iter.next(),
            Some(LocatedStr::new(
                Location::test_location(1, 1).into(),
                "asdf"
            ))
        );
        assert_eq!(segments_iter.next(), None);
    }

    #[test]
    fn segments_simple() {
        let under_test = ContentReference::new_from_node(test_concat_node(["abcd", "efg"]));

        let mut segments_iter = under_test.segments();
        assert_eq!(
            segments_iter.next(),
            Some(LocatedStr::new(
                Location::test_location(1, 1).into(),
                "abcd"
            ))
        );
        assert_eq!(
            segments_iter.next(),
            Some(LocatedStr::new(Location::test_location(1, 1).into(), "efg"))
        );
        assert_eq!(segments_iter.next(), None);
    }
}

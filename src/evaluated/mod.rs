//! The result of evaluating an AST
use crate::VariableName;
use std::sync::Arc;

#[cfg(test)]
mod test;

pub mod nodes;
mod nom;

// Reexport of the node interface
pub use self::nodes::*;

/// A fully evaluated block of text
#[derive(Clone, Default, Debug, PartialEq)]
pub struct Block {
    /// What variables this evaluation was sensitive to
    // #REQ-Expansion-Tracking
    sensitivity: fxhash::FxHashSet<VariableName>,
    /// The different fragments of content, tagged with how they came to be part of this block
    // #SPC-Variable-Eval.concat
    content: Vec<ContentReference>,
}

/// Represents a reference to some subsection of an evaluated node
#[derive(Clone, Debug, PartialEq)]
pub struct ContentReference {
    /// The offset of this reference into the node (in bytes)
    offset: usize,
    /// How much of the referenced node this reference cares about (in bytes)
    length: usize,
    /// The backing node
    node: Arc<EvaluatedNode>,
}

lazy_static::lazy_static!(
    static ref EMPTY_BLOCK: Arc<Block> = Default::default();
);

impl Block {
    /// An empty block
    pub(crate) fn empty() -> Arc<Block> {
        EMPTY_BLOCK.clone()
    }

    /// Create a new evaluated block
    pub(crate) fn new(
        sensitivity: fxhash::FxHashSet<VariableName>,
        content: Vec<ContentReference>,
    ) -> Arc<Block> {
        let &mut tr_block = Block {
            sensitivity,
            content,
        };
        tr_block.simplify();
        Arc::new(tr_block)
    }

    /// Flatten all the leaves into a single string.
    /// This discards all semantic information, so it's mostly useful for
    /// debugging and unit test assertions. If you want to retain semantic
    /// information, consider using one of visitor methods instead.
    pub fn into_string(&self) -> String {
        let mut buffer = String::new();
        for reference in &self.content {
            reference.append_to_string(&mut buffer);
        }
        buffer
    }

    /// Get the total length of this block in bytes
    pub fn len(&self) -> usize {
        self.content.iter().map(|x| x.length).sum()
    }

    /// Get this block as a parsable span
    pub fn span(&self) -> BlockSpan {
        let length = self.content.iter().map(|x| x.length).sum();
        BlockSpan {
            parent: self,
            contents: &self.content,
            offset: 0,
            length: length,
        }
    }

    /// Get a list of things that this tree is sensitive too
    pub fn sensitivity(&self) -> impl Iterator<Item = &VariableName> {
        self.sensitivity.iter()
    }

    /// Consume this tree, leaving behind an iterator over its sensitivities
    pub fn into_sensitivity(self) -> impl Iterator<Item = VariableName> {
        self.sensitivity.into_iter()
    }

    /// Push more content into this block
    pub fn push(&mut self, content: ContentReference) {
        self.content.push(content);
    }

    /// Simplify the contents of the block by reducing the size of concats and constants
    fn simplify(&mut self) {
        for reference in &mut self.content {
            reference.simplify()
        }
    }
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

    /// Create a content reference for a synthetically generated new line
    pub fn create_newline() -> Self {
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

    /// Simplify the node we're pointing to by dropping unnecessary components
    /// This means we rewrite constant nodes completely, realize offsets into concatenations,
    /// and recurse into concatenations
    fn simplify(&mut self) {
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
                eprintln!("{:?}", block.content);
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
pub struct ContentReferenceIter<'a>(std::iter::Take<std::iter::Skip<Chars<'a>>>);

impl<'a> Iterator for ContentReferenceIter<'a> {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        self.0.next()
    }
}

/// Represents a span of elements in an evaluated AST
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct BlockSpan<'a> {
    /// The parent bock
    parent: &'a Block,
    /// The slice of the parent's content we care about,
    contents: &'a [ContentReference],
    /// The offset into the first element, in bytes.
    /// NOTE: this is *in addition to* the offset in the reference itself
    offset: usize,
    /// The total length of all string contents
    length: usize,
}

impl<'a> BlockSpan<'a> {
    fn empty() -> BlockSpan<'static> {
        BlockSpan {
            parent: &EMPTY_BLOCK,
            contents: &[],
            offset: 0,
            length: 0,
        }
    }

    #[inline]
    fn revalidate_offset(&mut self) {
        if self.length == 0 {
            return;
        }

        let start_span_length = self.contents[0].length;
        while self.offset > start_span_length {
            self.offset -= start_span_length;
            self.contents = &self.contents[1..];
        }
    }

    /// Flatten this span into a string.
    pub fn into_string(&self) -> String {
        if self.length == 0 {
            return String::new();
        }

        let mut buffer = String::with_capacity(self.length);
        for element in self.chars() {
            buffer.push(element)
        }

        buffer
    }

    pub fn chars(&self) -> BlockSpanIter<'a> {
        BlockSpanIter::new(self)
    }

    pub fn to_new_block(&self) -> Arc<Block> {
        if self.length == 0 {
            return EMPTY_BLOCK.clone();
        }

        assert!(self.contents.len() > 0);
        assert!(self.offset < self.contents[0].length);

        let mut nodes = Vec::new();
        let mut contents_iter = self.contents.iter().map(|x| x.clone());
        // Unwrap is safe since we asserted above that contents length was > 0
        let mut to_push = contents_iter.next().unwrap();
        to_push.offset += self.offset;
        to_push.length -= self.offset;

        let mut remaining_length = self.length;

        while to_push.length < remaining_length {
            remaining_length -= to_push.length;
            nodes.push(to_push);
            to_push = contents_iter
                .next()
                .expect("ran out of contents before reaching the expected length");
        }
        assert!(to_push.length >= remaining_length);
        to_push.length = remaining_length;
        nodes.push(to_push);

        Block::new(self.parent.sensitivity().map(|x| *x).collect(), nodes)
    }
}

/// Iterator over a span of a block
#[derive(Clone, Debug)]
pub struct BlockSpanIter<'a> {
    cons_iter: std::slice::Iter<'a, ContentReference>,
    char_iter: Box<BlockSpanCharIteratorInternal<'a>>,
    remaining_bytes: usize,
}

#[derive(Clone, Debug)]
enum BlockSpanCharIteratorInternal<'a> {
    SkippedContentReference(std::iter::Skip<ContentReferenceIter<'a>>),
    ContentReference(ContentReferenceIter<'a>),
    NullIter(std::str::Chars<'a>),
}

impl<'a> BlockSpanIter<'a> {
    fn new(parent: &BlockSpan<'a>) -> Self {
        let mut cons_iter = parent.contents.iter();
        let char_iter: Box<BlockSpanCharIteratorInternal<'a>> = match cons_iter.next() {
            Some(x) => Box::new(BlockSpanCharIteratorInternal::SkippedContentReference(
                x.chars().skip(parent.offset),
            )),
            None => Box::new(BlockSpanCharIteratorInternal::NullIter("".chars())),
        };
        Self {
            cons_iter,
            char_iter,
            remaining_bytes: parent.length,
        }
    }
}

impl<'a> std::iter::Iterator for BlockSpanIter<'a> {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        use std::borrow::BorrowMut;

        if self.remaining_bytes == 0 {
            return None;
        }

        let mut tr = match self.char_iter.borrow_mut() {
            BlockSpanCharIteratorInternal::SkippedContentReference(it) => it.next(),
            BlockSpanCharIteratorInternal::ContentReference(it) => it.next(),
            BlockSpanCharIteratorInternal::NullIter(it) => it.next(),
        };
        while tr.is_none() {
            self.char_iter = Box::new(BlockSpanCharIteratorInternal::ContentReference(
                (*self.cons_iter.next()?).chars(),
            ));
            tr = match self.char_iter.borrow_mut() {
                BlockSpanCharIteratorInternal::SkippedContentReference(it) => it.next(),
                BlockSpanCharIteratorInternal::ContentReference(it) => it.next(),
                BlockSpanCharIteratorInternal::NullIter(it) => it.next(),
            };
        }
        // this unwrap is safe because we only reach here if tr.is_none() ==
        // false (i.e. tr is some)
        self.remaining_bytes -= tr.unwrap().len_utf8();
        tr
    }
}

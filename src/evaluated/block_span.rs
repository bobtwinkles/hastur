//! Implementation of the block span
use crate::evaluated::{Block, ContentReference, EvaluatedNode};
use std::sync::Arc;

/// Represents a span of elements in an evaluated AST
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct BlockSpan<'a> {
    /// The parent bock
    pub(super) parent: &'a Block,
    /// The slice of the parent's content we care about,
    pub(super) contents: &'a [ContentReference],
    /// The offset into the first element, in bytes.
    /// NOTE: this is *in addition to* the offset in the reference itself
    pub(super) offset: usize,
    /// The total length of all string contents
    pub(super) length: usize,
}

impl<'a> BlockSpan<'a> {
    pub(super) fn empty() -> BlockSpan<'static> {
        BlockSpan {
            parent: &super::EMPTY_BLOCK,
            contents: &[],
            offset: 0,
            length: 0,
        }
    }

    #[inline]
    pub(super) fn revalidate_offset(&mut self) {
        if self.length == 0 {
            return;
        }

        let mut start_span_length = self.contents[0].length;
        while self.offset >= start_span_length {
            self.offset -= start_span_length;
            self.contents = &self.contents[1..];
            start_span_length = self.contents[0].length;
        }
    }

    /// Get the length of this span
    pub fn len(&self) -> usize {
        self.length
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

    /// Get an iterator over the characters
    pub fn chars(&self) -> Iter<'a> {
        Iter::new(self)
    }

    /// Iterate over the character indices
    pub fn char_indices(&self) -> IndexIter<'a> {
        IndexIter::new(self)
    }

    /// Iterate over the actual segments of text in this block
    pub fn segments(&self) -> SegmentsIter<'a> {
        SegmentsIter::new(self)
    }

    /// Create a new owned block from this slice
    pub fn to_new_block(&self) -> Arc<Block> {
        if self.length == 0 {
            return super::EMPTY_BLOCK.clone();
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

    /// Create a new owned block from this slice, and package it up into a nice `ContentReference`
    pub fn to_content_reference(&self) -> ContentReference {
        ContentReference::new_from_node(Arc::new(EvaluatedNode::Concat(self.to_new_block())))
    }
}

/// Iterator over a span of a block
#[derive(Clone, Debug)]
pub struct Iter<'a> {
    cons_iter: std::slice::Iter<'a, ContentReference>,
    char_iter: Box<BlockSpanCharIteratorInternal<'a>>,
    remaining_bytes: usize,
}

#[derive(Clone, Debug)]
enum BlockSpanCharIteratorInternal<'a> {
    SkippedContentReference(std::iter::Skip<super::content_reference::ContentReferenceIter<'a>>),
    ContentReference(super::content_reference::ContentReferenceIter<'a>),
    NullIter(std::str::Chars<'a>),
}

impl<'a> Iter<'a> {
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

impl<'a> std::iter::FusedIterator for Iter<'a> {}

impl<'a> std::iter::Iterator for Iter<'a> {
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

/// Iterator over the span of a block
#[derive(Clone, Debug)]
pub struct IndexIter<'a> {
    char_iter: Iter<'a>,
    head: usize,
}

impl<'a> IndexIter<'a> {
    fn new(parent: &BlockSpan<'a>) -> Self {
        IndexIter {
            char_iter: parent.chars(),
            head: 0,
        }
    }
}
impl<'a> std::iter::FusedIterator for IndexIter<'a> {}

impl<'a> std::iter::Iterator for IndexIter<'a> {
    type Item = (usize, char);

    fn next(&mut self) -> Option<(usize, char)> {
        let tr_char = self.char_iter.next()?;
        let tr_head = self.head;
        self.head += tr_char.len_utf8();

        Some((tr_head, tr_char))
    }
}

/// Iterator over the actual contents of the block
#[derive(Clone, Debug)]
pub struct SegmentsIter<'a> {
    references_iter: std::slice::Iter<'a, ContentReference>,
    segments_iter: Box<Option<super::content_reference::SegmentsIter<'a>>>,
    offset: usize,
    length: usize,
}

impl<'a> SegmentsIter<'a> {
    fn new(parent: &BlockSpan<'a>) -> Self {
        let mut references_iter = parent.contents.iter();
        let segments_iter = Box::new(references_iter.next().map(|x| x.segments()));
        Self {
            offset: parent.offset,
            length: parent.length,
            references_iter,
            segments_iter,
        }
    }

    #[inline(always)]
    fn next_span(&mut self) -> Option<crate::source_location::LocatedStr<'a>> {
        use std::ops::DerefMut;

        if self.length == 0 {
            // Out of data we care about
            return None;
        }

        // Get the next segment
        let mut tr = match self.segments_iter.deref_mut() {
            Some(v) => v.next(),
            None => return None,
        };
        while tr == None {
            let mut segment_iter = self.references_iter.next()?.segments();
            tr = segment_iter.next();
            self.segments_iter.replace(segment_iter);
        }

        tr
    }
}

impl<'a> std::iter::FusedIterator for SegmentsIter<'a> {}

impl<'a> Iterator for SegmentsIter<'a> {
    type Item = crate::source_location::LocatedStr<'a>;

    fn next(&mut self) -> Option<crate::source_location::LocatedStr<'a>> {
        let mut tr = self.next_span()?;
        while tr.len() < self.offset {
            self.offset -= tr.len();
            tr = self.next_span()?;
        }
        if self.offset != 0 {
            tr = tr.slice(self.offset, tr.len() - self.offset);
        }
        self.offset = 0;
        if self.length < tr.len() {
            tr = tr.slice(0, self.length);
        }
        self.length -= tr.len();

        Some(tr)
    }
}

//! Various handy functions for traversing make string content

use crate::evaluated::BlockSpan;
use nom::{InputIter, Slice};
use std::iter::Peekable;

/// This is an iterator over a `BlockSpan`, chunked by Make tokens. Note that
/// these are not the same thing as the ones that our tokenizer produces. It
/// essentially means "whitespace separated"
pub(super) struct MakeTokens<'a> {
    span: BlockSpan<'a>,
    iterator: Peekable<<BlockSpan<'a> as InputIter>::Iter>,
}

impl<'a> Iterator for MakeTokens<'a> {
    type Item = BlockSpan<'a>;

    fn next(&mut self) -> Option<BlockSpan<'a>> {
        // First, skip any leftover whitespace
        while let Some((idx, chr)) = self.iterator.peek() {
            if !chr.is_whitespace() {
                break;
            }
            self.iterator.next();
        }

        let (start_idx, _) = self.iterator.next()?;

        while let Some((end_idx, chr)) = self.iterator.peek() {
            if chr.is_whitespace() {
                // end of this token
                return Some(self.span.slice(start_idx..*end_idx));
            }
            self.iterator.next();
        }

        Some(self.span.slice(start_idx..))
    }
}

pub(super) fn by_make_token(span: BlockSpan) -> MakeTokens {
    let iterator = span.iter_indices().peekable();
    MakeTokens { span, iterator }
}

// #SPC-Variable-Eval.nom
use super::{BlockSpan, BlockSpanIter};

use nom::{AtEof, Compare, InputIter, InputLength, InputTake, InputTakeAtPosition, Slice};
use nom::{CompareResult, Context, Err, ErrorKind, IResult};
use std::ops::{Range, RangeFrom, RangeTo};

#[cfg(test)]
mod test;

impl<'a> InputIter for BlockSpan<'a> {
    type Item = char;
    type RawItem = char;
    type Iter = std::iter::Enumerate<Self::IterElem>;
    type IterElem = BlockSpanIter<'a>;

    #[inline]
    fn iter_indices(&self) -> Self::Iter {
        self.iter_elements().enumerate()
    }

    #[inline]
    fn iter_elements(&self) -> Self::IterElem {
        self.chars()
    }

    #[inline]
    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.iter_elements().position(predicate)
    }

    #[inline]
    fn slice_index(&self, count: usize) -> Option<usize> {
        if self.length >= count {
            Some(count)
        } else {
            None
        }
    }
}

impl<'a> AtEof for BlockSpan<'a> {
    fn at_eof(&self) -> bool {
        true
    }
}

impl<'a> Slice<Range<usize>> for BlockSpan<'a> {
    fn slice(&self, range: Range<usize>) -> Self {
        let mut tr = self.clone();
        tr.offset += range.start;
        tr.length = range.end - range.start;
        tr.revalidate_offset();

        tr
    }
}

impl<'a> Slice<RangeFrom<usize>> for BlockSpan<'a> {
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        let mut tr = self.clone();
        tr.offset += range.start;
        tr.length -= range.start;
        tr.revalidate_offset();

        tr
    }
}

impl<'a> Slice<RangeTo<usize>> for BlockSpan<'a> {
    fn slice(&self, range: RangeTo<usize>) -> Self {
        let mut tr = self.clone();
        tr.length = range.end;

        tr
    }
}

impl<'a> InputTakeAtPosition for BlockSpan<'a> {
    type Item = char;

    fn split_at_position<P>(&self, predicate: P) -> IResult<Self, Self, u32>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.iter_indices().find(|&(_, c)| predicate(c)) {
            Some((i, _)) => Ok((self.slice(i..), self.slice(..i))),
            None => Ok((BlockSpan::empty(), *self)),
        }
    }

    fn split_at_position1<P>(&self, predicate: P, e: ErrorKind<u32>) -> IResult<Self, Self, u32>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.iter_indices().find(|&(_, c)| predicate(c)) {
            Some((0, _)) => Err(Err::Error(Context::Code(*self, e))),
            Some((i, _)) => Ok((self.slice(i..), self.slice(..i))),
            None => {
                if self.length == 0 {
                    Err(Err::Error(Context::Code(*self, e)))
                } else {
                    Ok((BlockSpan::empty(), *self))
                }
            }
        }
    }
}

impl<'a> InputTake for BlockSpan<'a> {
    #[inline]
    fn take(&self, count: usize) -> Self {
        assert!(count <= self.length);

        self.slice(..count)
    }

    #[inline]
    fn take_split(&self, count: usize) -> (Self, Self) {
        assert!(count <= self.length);

        (self.slice(count..), self.slice(..count))
    }
}

impl<'a> InputLength for BlockSpan<'a> {
    fn input_len(&self) -> usize {
        self.length
    }
}

impl<'a, 'b> Compare<&'b str> for BlockSpan<'a> {
    fn compare(&self, t: &'b str) -> CompareResult {
        let pos = self
            .iter_elements()
            .zip(t.chars())
            .position(|(a, b)| a != b);

        match pos {
            Some(_) => CompareResult::Error,
            None => {
                if self.length >= t.len() {
                    CompareResult::Ok
                } else {
                    CompareResult::Incomplete
                }
            }
        }
    }

    fn compare_no_case(&self, t: &'b str) -> CompareResult {
        let pos = self
            .iter_elements()
            .flat_map(|c| c.to_lowercase())
            .zip(t.chars().flat_map(|c| c.to_lowercase()))
            .position(|(a, b)| a != b);

        match pos {
            Some(_) => CompareResult::Error,
            None => {
                if self.length >= t.len() {
                    CompareResult::Ok
                } else {
                    CompareResult::Incomplete
                }
            }
        }
    }
}

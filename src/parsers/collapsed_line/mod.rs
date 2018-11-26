use super::{CollapsedLineSpan, Span};
use nom::*;
use std::ops::{Range, RangeFrom, RangeTo};

#[cfg(test)]
mod test;

impl<'a, 'line: 'a> CollapsedLineSpan<'a, 'line> {
    #[inline]
    fn revalidate_offset(&mut self) {
        while self.parent.len() > 0 && self.offset >= self.parent[0].fragment.len() {
            self.offset -= self.parent[0].fragment.len();
            self.parent = &self.parent[1..];
        }
    }
}

impl<'a, 'line: 'a> AtEof for CollapsedLineSpan<'a, 'line> {
    #[inline]
    fn at_eof(&self) -> bool {
        true
    }
}

impl<'a, 'line: 'a> Slice<Range<usize>> for CollapsedLineSpan<'a, 'line> {
    fn slice(&self, range: Range<usize>) -> Self {
        let mut tr = self.clone();
        tr.offset += range.start;
        tr.length = range.end - range.start;
        tr.revalidate_offset();

        tr
    }
}

impl<'a, 'line: 'a> Slice<RangeFrom<usize>> for CollapsedLineSpan<'a, 'line> {
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        let mut tr = self.clone();
        tr.offset += range.start;
        tr.length -= range.start;
        tr.revalidate_offset();

        tr
    }
}

impl<'a, 'line: 'a> Slice<RangeTo<usize>> for CollapsedLineSpan<'a, 'line> {
    fn slice(&self, range: RangeTo<usize>) -> Self {
        let mut tr = self.clone();
        tr.length = range.end;

        tr
    }
}

impl<'a, 'line: 'a> InputTakeAtPosition for CollapsedLineSpan<'a, 'line> {
    type Item = char;

    fn split_at_position<P>(&self, predicate: P) -> IResult<Self, Self, u32>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.iter_indices().find(|&(_, c)| predicate(c)) {
            Some((i, _)) => Ok((self.slice(i..), self.slice(..i))),
            None => Ok((CollapsedLineSpan::empty(), *self)),
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
                    Ok((CollapsedLineSpan::empty(), *self))
                }
            }
        }
    }
}

impl<'a, 'line: 'a> InputTake for CollapsedLineSpan<'a, 'line> {
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

impl<'a, 'line: 'a> InputLength for CollapsedLineSpan<'a, 'line> {
    fn input_len(&self) -> usize {
        self.length
    }
}

impl<'a, 'line: 'a, 'b> Compare<&'b str> for CollapsedLineSpan<'a, 'line> {
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

pub(crate) struct CollapsedLineIter<'a, 'line: 'a> {
    line_iter: std::slice::Iter<'a, Span<'line>>,
    char_iter: std::str::Chars<'a>,
    remaining_bytes: usize,
}

impl<'a, 'line: 'a> CollapsedLineIter<'a, 'line> {
    fn new(parent: &CollapsedLineSpan<'a, 'line>) -> Self {
        let mut line_iter = (parent.parent).iter();
        let char_iter = match line_iter.next() {
            Some(x) => x.fragment[parent.offset..].chars(),
            None => "".chars(),
        };
        Self {
            line_iter,
            char_iter,
            remaining_bytes: parent.length,
        }
    }
}

impl<'a, 'line: 'a> std::iter::Iterator for CollapsedLineIter<'a, 'line> {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        if self.remaining_bytes == 0 {
            return None;
        }
        let mut tr = self.char_iter.next();
        while tr.is_none() {
            self.char_iter = self.line_iter.next()?.fragment.chars();
            tr = self.char_iter.next();
        }
        // this unwrap is safe because we only reach here if tr.is_none() ==
        // false (i.e. tr is some)
        self.remaining_bytes -= tr.unwrap().len_utf8();
        tr
    }
}

impl<'a, 'line: 'a> InputIter for CollapsedLineSpan<'a, 'line> {
    type Item = char;
    type RawItem = char;
    type Iter = std::iter::Enumerate<Self::IterElem>;
    type IterElem = CollapsedLineIter<'a, 'line>;

    #[inline]
    fn iter_indices(&self) -> Self::Iter {
        self.iter_elements().enumerate()
    }

    #[inline]
    fn iter_elements(&self) -> Self::IterElem {
        CollapsedLineIter::new(self)
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

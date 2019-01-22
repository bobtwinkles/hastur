// #SPC-Variable-Eval.nom
use super::BlockSpan;

use nom::{
    AtEof, Compare, FindSubstring, InputIter, InputLength, InputTake, InputTakeAtPosition, Slice,
};
use nom::{CompareResult, Context, Err, ErrorKind, IResult};
use std::ops::{Range, RangeFrom, RangeTo};

#[cfg(test)]
mod test;

impl<'a> InputIter for BlockSpan<'a> {
    type Item = char;
    type RawItem = char;
    type Iter = super::block_span::IndexIter<'a>;
    type IterElem = super::block_span::Iter<'a>;

    #[inline]
    fn iter_indices(&self) -> Self::Iter {
        self.char_indices()
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
            Some((i, _)) => {
                let fst = self.slice(i..);
                let snd = self.slice(..i);
                Ok((fst, snd))
            }
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

impl<'a, 'b> FindSubstring<&'b str> for BlockSpan<'a> {
    fn find_substring(&self, substr: &'b str) -> Option<usize> {
        // If the substring is longer than we are, there's no hope
        if substr.len() > self.len() {
            return None;
        }

        let substr_bytes = substr.as_bytes();
        let mut buffer = Vec::with_capacity(substr.len());
        let mut char_buffer = [0, 0, 0, 0];
        buffer.resize_with(substr.len(), || 0);

        let mut self_iter = self.iter_indices();
        let mut self_idx = 0;
        let mut buffer_start = 0;

        while self_idx < substr.len() {
            let content = self_iter.next().unwrap();
            let char_size = content.1.encode_utf8(&mut char_buffer).len();
            for i in 0..char_size {
                buffer[self_idx + i] = char_buffer[i];
            }
            self_idx = content.0 + char_size;
        }

        if buffer == substr.as_bytes() {
            return Some(0);
        }

        for (i, c) in self_iter {
            eprintln!(
                "Inspecting {:?} for match with {:?} {:?}",
                c,
                substr.as_bytes(),
                buffer
            );
            // Update the buffer
            let char_size = c.encode_utf8(&mut char_buffer).len();
            for j in 0..char_size {
                buffer[(buffer_start + j) % substr.len()] = char_buffer[j];
            }
            buffer_start = (buffer_start + char_size) % substr.len();
            // attempt to match the current buffer against the substring
            let mut all_match = true;
            for j in 0..substr.len() {
                if buffer[(buffer_start + j) % substr.len()] != substr_bytes[j] {
                    all_match = false;
                    break;
                }
            }
            if all_match {
                eprintln!("Match found at {:?}", i - substr.len());
                return Some(i - substr.len() + 1);
            }
        }

        None
    }
}

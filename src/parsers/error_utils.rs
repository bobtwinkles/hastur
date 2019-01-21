use crate::evaluated::BlockSpan;
use nom::{Context, Err};

/// Lifts an error that occurred while processing a collapsed line to an error
/// on a span that line contains
#[inline]
pub(crate) fn lift_collapsed_span_error<'a, 'b, E>(
    e: Err<BlockSpan<'a>, E>,
    location_injection: BlockSpan<'b>,
) -> Err<BlockSpan<'b>, E> {
    macro_rules! map_context {
        ($e:expr) => {
            match $e {
                Context::Code(loc, e) => Context::Code(location_injection, e),
                Context::List(errs) => Context::List(
                    errs.into_iter()
                        .map(|(loc, e)| (location_injection, e))
                        .collect(),
                ),
            }
        };
    }
    match e {
        Err::Incomplete(x) => Err::Incomplete(x),
        Err::Failure(e) => Err::Failure(map_context!(e)),
        Err::Error(e) => Err::Error(map_context!(e)),
    }
}

pub(crate) trait NomErrExt<I, E> {
    /// Add a new error to the error context
    /// Incompletes become failures
    fn add_error(self, i: I, e: nom::ErrorKind<E>) -> Self;

    /// Turns an error into a failure
    /// The error must be not be an incomplete
    fn as_failure(self) -> Self;
}

impl<I, E> NomErrExt<I, E> for Err<I, E> {
    fn add_error(self, i: I, e: nom::ErrorKind<E>) -> Self {
        macro_rules! add_error {
            ($c:expr) => {{
                let mut v = match $c {
                    Context::List(v) => v,
                    Context::Code(i, e) => {
                        let mut v = Vec::new();
                        v.push((i, e));
                        v
                    }
                };

                v.push((i, e));
                Context::List(v)
            }};
        }
        match self {
            Err::Incomplete(_) => Err::Failure(Context::Code(i, e)),
            Err::Failure(c) => Err::Failure(add_error!(c)),
            Err::Error(c) => Err::Error(add_error!(c)),
        }
    }

    fn as_failure(self) -> Self {
        match self {
            Err::Incomplete(_) => panic!("Error cannot be an incomplete"),
            Err::Failure(e) | Err::Error(e) => Err::Failure(e),
        }
    }
}

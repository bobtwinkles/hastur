//! Macros that operate using the ParseErrorKind

/// A variant of nom's `complete!` macro that doesn't tie itself to `<i32>` in the error kind.
#[macro_export]
macro_rules! pe_complete {
    ($i:expr, $submac:ident!( $($args:tt)* )) => (
        {
            use std::result::Result::*;
            use nom::{Err,ErrorKind};

            let i_ = $i.clone();
            match $submac!(i_, $($args)*) {
                Err(Err::Incomplete(_)) =>  {
                    Err(Err::Error(error_position!($i, ErrorKind::Complete::<ParseErrorKind>)))
                },
                rest => rest
            }
        }
    );
    ($i:expr, $f:expr) => (
        pe_complete!($i, call!($f));
    );
}

/// Shorthand for `fix_error!(crate::ParseErrorKind, ...)`
#[macro_export]
macro_rules! pe_fix {
    ($i:expr, $submac:ident ! ( $($args:tt)* )) => (
        fix_error!($i, crate::ParseErrorKind, $submac!($($args)*))
    );
    ($i:expr, $f:expr) => (
        pe_fix!($i, call!($f))
    );
}

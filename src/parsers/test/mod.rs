//! Test utilities and integration tests

use super::*;
use nom::Context as NContext;
use nom::{Err, ErrorKind};

/// Create a span for use in testing, usually from a static string
pub(super) fn create_span<'a>(s: &'a str) -> Span<'a> {
    crate::Span::new(nom::types::CompleteStr(s))
}

/// Create a span that is setup for equality comparison after running a parser
pub(super) fn leftover_span<'a>(fragment: &'a str, offset: usize, line: u32) -> Span<'a> {
    let mut tr = create_span(fragment);
    tr.offset = offset;
    tr.line = line;

    tr
}

pub(super) fn simple_line(s: & str) -> CollapsedLine {
    super::makefile_line(create_span(s), super::ParserCompliance::GNU, false).unwrap().1
}

pub(super) fn result_owned_fragment<T>(
    fragment: impl AsRef<str>,
    offset: usize,
    line: u32,
    file_name: T,
) -> OwnedFragment<T> {
    let span = leftover_span(fragment.as_ref(), offset, line);
    OwnedFragment::from_span(span, file_name)
}

pub(super) fn error_context<I, E>(e: Err<I, E>) -> Option<NContext<I, E>> {
    match e {
        Err::Incomplete(_) => None,
        Err::Error(c) => Some(c),
        Err::Failure(c) => Some(c),
    }
}

pub(super) fn error_list_contains<I, E: PartialEq>(
    es: &Vec<(I, ErrorKind<E>)>,
    needle: ErrorKind<E>,
) -> bool {
    for (_, error) in es {
        if *error == needle {
            return true;
        }
    }
    return false;
}

pub(super) fn error_list_contains_at_location<I: PartialEq, E: PartialEq>(
    es: &Vec<(I, ErrorKind<E>)>,
    error_needle: ErrorKind<E>,
    location_needle: I,
) -> bool {
    for (location, error) in es {
        if *error == error_needle && *location == location_needle {
            return true;
        }
    }
    return false;
}

#[test]
fn test_ends_with_backslash() {
    // use std::io::Cursor;

    assert_eq!(ends_with_backslash(""), 0);
    assert_eq!(ends_with_backslash("\\"), 1);
    assert_eq!(ends_with_backslash("\\\\"), 2);
    assert_eq!(ends_with_backslash("\\\\\\"), 3);
    assert_eq!(ends_with_backslash("asdf\\\\\\"), 3);
    assert_eq!(ends_with_backslash("asdf\\\\"), 2);
}

mod makefile_line {
    use crate::parsers::{makefile_line, CollapsedLine};
    use crate::parsers::test::*;

    #[test]
    fn simple() {
        let test_span = create_span("just a line");
        for mode in &[ParserCompliance::GNU, ParserCompliance::POSIX] {
            for strip_whitespace in &[false, true] {
                eprintln!("Testing mode {:?} {:?}", mode, strip_whitespace);
                let parse = makefile_line(test_span, *mode, *strip_whitespace);

                assert_eq!(
                    parse,
                    Ok((
                        leftover_span("", 11, 1),
                        CollapsedLine::new(vec![test_span])
                    ))
                );
            }
        }
    }

    #[test]
    fn simple_collapse() {
        let test_span = create_span("line 1\\\nline 2");
        let parse = makefile_line(test_span, ParserCompliance::POSIX, false);

        assert_eq!(
            parse,
            Ok((
                leftover_span("", 14, 2),
                CollapsedLine::new(vec![
                    leftover_span("line 1", 0, 1),
                    leftover_span(" ", 8, 2),
                    leftover_span("line 2", 8, 2),
                ])
            ))
        );
    }

    #[test]
    fn collapse_preserves() {
        let test_span = create_span("line 1\t\\\nline 2");
        let parse = makefile_line(test_span, ParserCompliance::GNU, false);

        assert_eq!(
            parse,
            Ok((
                leftover_span("", 15, 2),
                CollapsedLine::new(vec![
                    leftover_span("line 1", 0, 1),
                    leftover_span(" ", 9, 2),
                    leftover_span("line 2", 9, 2)
                ])
            ))
        );
    }

    #[test]
    fn strip_initial_works() {
        let test_span = create_span(" line 1\\\n\tline 2");
        let parse = makefile_line(test_span, ParserCompliance::GNU, true);

        assert_eq!(
            parse,
            Ok((
                leftover_span("", 16, 2),
                CollapsedLine::new(vec![
                    leftover_span("line 1", 1, 1),
                    leftover_span(" ", 9, 2),
                    leftover_span("line 2", 10, 2),
                ])
            ))
        );
    }

    #[test]
    fn strip_initial_works_multiple() {
        let test_span = create_span("line 1\\\n\t\\\nline 2");
        let parse = makefile_line(test_span, ParserCompliance::GNU, true);

        assert_eq!(
            parse,
            Ok((
                leftover_span("", 17, 3),
                CollapsedLine::new(vec![
                    leftover_span("line 1", 0, 1),
                    leftover_span(" ", 8, 2),
                    leftover_span("line 2", 11, 3),
                ])
            ))
        );
    }

    #[test]
    fn test_simple_end() {
        let test_span = create_span(r"\\");
        let parse = makefile_line(test_span, ParserCompliance::POSIX, true);

        assert_eq!(
            parse,
            Ok((
                leftover_span("", 2, 1),
                CollapsedLine::new(vec![create_span(r"\\")])
            ))
        );
    }

    #[test]
    fn strip_initial_line_break() {
        let test_span = create_span(" \t\\\n\t a");
        let parse = makefile_line(test_span, ParserCompliance::POSIX, true);

        assert_eq!(
            parse,
            Ok((
                leftover_span("", 7, 2),
                CollapsedLine::new(vec![leftover_span(r"a", 6, 2)])
            ))
        );
    }

    // TODO: test some more sophisticated collapse logic. POSIX/GNU compliance?
}

mod makefile_whitespace {
    use crate::parsers::makefile_whitespace;
    use crate::parsers::test::*;

    #[test]
    fn single_space() {
        let test_span = create_span(" ");
        let parse = makefile_whitespace(test_span);

        assert!(parse.is_ok());
        let (leftover, _) = parse.ok().unwrap();
        assert_eq!(leftover, leftover_span("", 1, 1));
    }

    #[test]
    fn single_tab() {
        let test_span = create_span("\t");
        let parse = makefile_whitespace(test_span);

        assert!(parse.is_ok());
        let (leftover, _) = parse.ok().unwrap();
        assert_eq!(leftover, leftover_span("", 1, 1));
    }

    #[test]
    fn multiple_space() {
        let test_span = create_span("  ");
        let parse = makefile_whitespace(test_span);

        assert!(parse.is_ok());
        let (leftover, _) = parse.ok().unwrap();
        assert_eq!(leftover, leftover_span("", 2, 1));
    }

    #[test]
    fn mixed_mode() {
        let test_span = create_span(" \t");
        let parse = makefile_whitespace(test_span);

        assert!(parse.is_ok());
        let (leftover, _) = parse.ok().unwrap();
        assert_eq!(leftover, leftover_span("", 2, 1));
    }

    #[test]
    fn conservative() {
        let test_span = create_span(" a");
        let parse = makefile_whitespace(test_span);

        assert!(parse.is_ok());
        let (leftover, _) = parse.ok().unwrap();
        assert_eq!(leftover, leftover_span("a", 1, 1));
    }

    #[test]
    fn conservative_mixed() {
        let test_span = create_span(" \t a");
        let parse = makefile_whitespace(test_span);

        assert!(parse.is_ok());
        let (leftover, _) = parse.ok().unwrap();
        assert_eq!(leftover, leftover_span("a", 3, 1));
    }
}

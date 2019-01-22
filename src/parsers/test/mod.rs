//! Test utilities and integration tests

use super::*;
use crate::evaluated::test::concat_node_with_locations;
use crate::source_location::Location;
use nom::Context as NContext;
use nom::{Err, ErrorKind};

/// Create a span for use in testing, usually from a static string
pub(super) fn create_span(s: &str) -> Arc<Block> {
    leftover_span(s, 1, 1)
}

/// Create a span that is setup for equality comparison after running a parser
pub(super) fn leftover_span(fragment: &str, character: u32, line: u32) -> Arc<Block> {
    let tr = Block::new(
        Default::default(),
        vec![crate::evaluated::ContentReference::new_from_node(Arc::new(
            EvaluatedNode::Constant(crate::source_location::LocatedString::new(
                crate::source_location::Location::TestLocation {
                    line: line,
                    character: character,
                }
                .into(),
                fragment.into(),
            )),
        ))],
    );

    tr
}

pub(super) fn assert_ok<T, E: std::fmt::Debug>(r: Result<T, E>) -> T {
    match r {
        Ok(v) => v,
        Err(e) => panic!("Unexpected error {:?}", e),
    }
}

pub(super) fn simple_line(s: &str) -> Arc<Block> {
    super::makefile_line(create_span(s).span(), super::ParserCompliance::GNU, false)
        .unwrap()
        .1
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

    assert_eq!(ends_with_backslash(create_span("").span()), 0);
    assert_eq!(ends_with_backslash(create_span("\\").span()), 1);
    assert_eq!(ends_with_backslash(create_span("\\\\").span()), 2);
    assert_eq!(ends_with_backslash(create_span("\\\\\\").span()), 3);
    assert_eq!(ends_with_backslash(create_span("asdf\\\\\\").span()), 3);
    assert_eq!(ends_with_backslash(create_span("asdf\\\\").span()), 2);
}

mod makefile_line {
    use crate::parsers::makefile_line;
    use crate::parsers::test::*;

    #[test]
    fn simple() {
        let test_span = create_span("just a line");
        let test_span = test_span.span();
        for mode in &[ParserCompliance::GNU, ParserCompliance::POSIX] {
            for strip_whitespace in &[false, true] {
                eprintln!("Testing mode {:?} {:?}", mode, strip_whitespace);
                let parse = makefile_line(test_span, *mode, *strip_whitespace);

                assert_eq!(
                    parse,
                    Ok((leftover_span("", 11, 1).span(), test_span.to_new_block()))
                );
            }
        }
    }

    #[test]
    fn simple_collapse() {
        let test_span = create_span("line 1\\\nline 2");
        let test_span = test_span.span();
        let parse = makefile_line(test_span, ParserCompliance::POSIX, false);

        assert_eq!(
            parse,
            Ok((
                leftover_span("", 14, 2).span(),
                concat_node_with_locations(&[
                    ("line 1", Location::test_location(1, 1)),
                    (" ", Location::Synthetic),
                    ("line 2", Location::test_location(2, 1)),
                ])
            ))
        );
    }

    #[test]
    fn collapse_preserves() {
        let test_span = create_span("line 1\t\\\nline 2");
        let test_span = test_span.span();
        let parse = makefile_line(test_span, ParserCompliance::GNU, false);

        assert_eq!(
            parse,
            Ok((
                leftover_span("", 15, 2).span(),
                concat_node_with_locations(&[
                    ("line 1", Location::test_location(1, 1)),
                    (" ", Location::Synthetic),
                    ("line 2", Location::test_location(2, 1)),
                ])
            ))
        );
    }

    #[test]
    fn strip_initial_works() {
        let test_span = create_span(" line 1\\\n\tline 2");
        let test_span = test_span.span();
        let parse = makefile_line(test_span, ParserCompliance::GNU, true);

        assert_eq!(
            parse,
            Ok((
                leftover_span("", 16, 2).span(),
                concat_node_with_locations(&[
                    ("line 1", Location::test_location(1, 1)),
                    (" ", Location::Synthetic),
                    ("line 2", Location::test_location(2, 1)),
                ])
            ))
        );
    }

    #[test]
    fn strip_initial_works_multiple() {
        let test_span = create_span("line 1\\\n\t\\\nline 2");
        let test_span = test_span.span();
        let parse = makefile_line(test_span, ParserCompliance::GNU, true);

        assert_eq!(
            parse,
            Ok((
                leftover_span("", 17, 3).span(),
                concat_node_with_locations(&[
                    ("line 1", Location::test_location(1, 1)),
                    (" ", Location::Synthetic),
                    ("line 2", Location::test_location(2, 1)),
                ])
            ))
        );
    }

    #[test]
    fn test_simple_end() {
        let test_span = create_span(r"\\");
        let test_span = test_span.span();
        let parse = makefile_line(test_span, ParserCompliance::POSIX, true);

        assert_eq!(
            parse,
            Ok((
                leftover_span("", 2, 1).span(),
                concat_node_with_locations(&[("\\", Location::test_location(1, 1))])
            ))
        );
    }

    #[test]
    fn strip_initial_line_break() {
        let test_span = create_span(" \t\\\n\t a");
        let test_span = test_span.span();
        let parse = makefile_line(test_span, ParserCompliance::POSIX, true);

        assert_eq!(
            parse,
            Ok((
                leftover_span("", 7, 2).span(),
                concat_node_with_locations(&[("a", Location::test_location(2, 1))])
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
        let test_span = test_span.span();
        let parse = makefile_whitespace(test_span);

        assert!(parse.is_ok());
        let (leftover, _) = parse.ok().unwrap();
        assert_eq!(leftover, leftover_span("", 1, 1).span());
    }

    #[test]
    fn single_tab() {
        let test_span = create_span("\t");
        let test_span = test_span.span();
        let parse = makefile_whitespace(test_span);

        assert!(parse.is_ok());
        let (leftover, _) = parse.ok().unwrap();
        assert_eq!(leftover, leftover_span("", 1, 1).span());
    }

    #[test]
    fn multiple_space() {
        let test_span = create_span("  ");
        let test_span = test_span.span();
        let parse = makefile_whitespace(test_span);

        assert!(parse.is_ok());
        let (leftover, _) = parse.ok().unwrap();
        assert_eq!(leftover, leftover_span("", 2, 1).span());
    }

    #[test]
    fn mixed_mode() {
        let test_span = create_span(" \t");
        let test_span = test_span.span();
        let parse = makefile_whitespace(test_span);

        assert!(parse.is_ok());
        let (leftover, _) = parse.ok().unwrap();
        assert_eq!(leftover, leftover_span("", 2, 1).span());
    }

    #[test]
    fn conservative() {
        let test_span = create_span(" a");
        let test_span = test_span.span();
        let parse = makefile_whitespace(test_span);

        assert!(parse.is_ok());
        let (leftover, _) = parse.ok().unwrap();
        assert_eq!(leftover, leftover_span("a", 1, 1).span());
    }

    #[test]
    fn conservative_mixed() {
        let test_span = create_span(" \t a");
        let test_span = test_span.span();
        let parse = makefile_whitespace(test_span);

        assert!(parse.is_ok());
        let (leftover, _) = parse.ok().unwrap();
        assert_eq!(leftover, leftover_span("a", 3, 1).span());
    }
}

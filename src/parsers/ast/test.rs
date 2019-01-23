//! Testign for the AST parser

use super::*;
use crate::parsers::test::create_span;
use crate::source_location::{LocatedString, Location};

#[test]
fn simple() {
    let block = create_span("ab");
    let res = assert_ok!(parse_ast(block.span()));

    assert_complete!(res.0);
    assert_eq!(
        res.1,
        ast::constant(
            Location::test_location(1, 1),
            LocatedString::new(Location::test_location(1, 1).into(), "ab".into())
        )
    )
}

#[test]
fn single_char_ref() {
    let block = create_span("$a");
    let res = assert_ok!(parse_ast(block.span()));

    assert_complete!(res.0);
    assert_eq!(
        res.1,
        ast::variable_reference(
            Location::test_location(1, 1),
            ast::constant(
                Location::test_location(1, 2),
                LocatedString::new(Location::test_location(1, 2).into(), "a".into())
            )
        )
    )
}

#[test]
fn dollar_at_end() {
    let block = create_span("$");
    let res = assert_ok!(parse_ast(block.span()));

    assert_complete!(res.0);
    assert_eq!(
        res.1,
        ast::constant(
            Location::test_location(1, 1),
            LocatedString::new(Location::test_location(1, 1).into(), "$".into())
        )
    );
}

#[test]
fn dollar_escape() {
    let block = create_span("$$");
    let res = assert_ok!(parse_ast(block.span()));

    assert_complete!(res.0);
    assert_eq!(
        res.1,
        ast::constant(
            Location::test_location(1, 1),
            LocatedString::new(Location::test_location(1, 2).into(), "$".into())
        )
    );
}

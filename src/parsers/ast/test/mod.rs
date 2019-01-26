//! Testign for the AST parser

use super::*;
use crate::parsers::test::{create_span, error_context, error_list_contains};
use crate::source_location::{LocatedString, Location};

mod arguments;
mod strip;
mod word;
mod words;
mod substitution_reference;

#[test]
fn simple() {
    let block = create_span("ab");
    let res = assert_ok!(parse_ast(block.span()));

    assert_complete!(res.0);
    assert_eq!(
        res.1,
        ast::constant(
            Location::test_location(1, 1),
            LocatedString::test_new(1, 1, "ab")
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
                LocatedString::test_new(1, 2, "a")
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
            LocatedString::test_new(1, 1, "$")
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
            LocatedString::test_new(1, 2, "$")
        )
    );
}

#[test]
fn long_var_name() {
    let block = create_span("$(foo)");
    let res = assert_ok!(parse_ast(block.span()));

    assert_complete!(res.0);
    assert_eq!(
        res.1,
        ast::variable_reference(
            Location::test_location(1, 1),
            ast::constant(
                Location::test_location(1, 3),
                LocatedString::test_new(1, 3, "foo")
            )
        )
    );
}

#[test]
fn recursive_variable_expansion() {
    let block = create_span("$($(foo))");
    let res = assert_ok!(parse_ast(block.span()));

    assert_complete!(res.0);
    assert_eq!(
        res.1,
        ast::variable_reference(
            Location::test_location(1, 1),
            ast::variable_reference(
                Location::test_location(1, 3),
                ast::constant(
                    Location::test_location(1, 5),
                    LocatedString::test_new(1, 5, "foo")
                )
            )
        )
    );
}

#[test]
fn unbalanced_reference() {
    let block = create_span("$(foo");
    let err = assert_err!(parse_ast(block.span()));
    assert_err_contains!(err, ParseErrorKind::UnternimatedVariable);
}

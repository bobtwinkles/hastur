//! Testign for the AST parser

use super::*;
use crate::parsers::test::create_span;
use crate::source_location::{LocatedString, Location};

macro_rules! test_setup {
    ($block_contents:expr, $name:ident, ok) => {
        crate::test::setup();

        let block = crate::parsers::test::create_span($block_contents);
        let $name = assert_ok!(crate::parsers::ast::parse_ast(block.span()));
    };
    ($block_contents:expr, $name:ident, err) => {
        crate::test::setup();

        let block = crate::parsers::test::create_span($block_contents);
        let $name = assert_err!(crate::parsers::ast::parse_ast(block.span()));
    };
}

// mod arguments;

mod eval;
mod if_fn;
mod proptest;
mod strip;
mod word;
mod words;

#[test]
fn simple() {
    let block = create_span("ab");
    let res = assert_ok!(parse_ast(block.span()));

    assert_complete!(res.0);
    assert_eq!(res.1, ast::constant(LocatedString::test_new(1, 1, "ab")))
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
            ast::constant(LocatedString::test_new(1, 2, "a"))
        )
    )
}

#[test]
fn single_char_ref_abutting() {
    test_setup!("$ab", res, ok);

    assert_complete!(res.0);
    assert_eq!(
        res.1,
        ast::collapsing_concat(
            Location::test_location(1, 1),
            vec![
                ast::variable_reference(
                    Location::test_location(1, 1),
                    ast::constant(LocatedString::test_new(1, 2, "a"))
                ),
                ast::constant(LocatedString::test_new(1, 3, "b"))
            ]
        )
    )
}

#[test]
fn dollar_at_end() {
    let block = create_span("$");
    let res = assert_ok!(parse_ast(block.span()));

    assert_complete!(res.0);
    assert_eq!(res.1, ast::constant(LocatedString::test_new(1, 1, "$")));
}

#[test]
fn dollar_escape() {
    test_setup!("$$", res, ok);
    assert_complete!(res.0);
    assert_eq!(res.1, ast::constant(LocatedString::test_new(1, 2, "$")));
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
            ast::constant(LocatedString::test_new(1, 3, "foo"))
        )
    );
}

#[test]
fn recursive_variable_expansion() {
    test_setup!("$($(foo))", res, ok);
    assert_complete!(res.0);
    assert_eq!(
        res.1,
        ast::variable_reference(
            Location::test_location(1, 1),
            ast::variable_reference(
                Location::test_location(1, 3),
                ast::constant(LocatedString::test_new(1, 5, "foo"))
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

#[test]
fn function_named_variable() {
    test_setup!("$(words)", parse, ok);
    assert_complete!(parse.0);
    assert_eq!(
        parse.1,
        ast::variable_reference(
            Location::test_location(1, 1),
            ast::constant(LocatedString::test_new(1, 3, "words"))
        )
    )
}

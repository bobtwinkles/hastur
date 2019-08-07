//! Testing for the if function parser

use pretty_assertions::assert_eq;

use crate::ast;
use crate::source_location::{LocatedString, Location};
use crate::ParseErrorKind;

// use crate::parsers::ast::test::test_setup;

#[test]
fn simple() {
    test_setup!("$(if t,a,b)", parse, ok);

    assert_complete!(parse.0);
    assert_eq!(
        parse.1,
        ast::if_fn(
            Location::test_location(1, 1),
            ast::constant(LocatedString::test_new(1, 6, "t")),
            ast::constant(LocatedString::test_new(1, 8, "a")),
            ast::constant(LocatedString::test_new(1, 10, "b")),
        )
    )
}

#[test]
fn two_args() {
    test_setup!("$(if t,a)", parse, ok);

    assert_complete!(parse.0);
    assert_eq!(
        parse.1,
        ast::if_fn(
            Location::test_location(1, 1),
            ast::constant(LocatedString::test_new(1, 6, "t")),
            ast::constant(LocatedString::test_new(1, 8, "a")),
            ast::constant(LocatedString::test_new(1, 10, "b")),
        )
    )
}

#[test]
fn one_arg() {
    test_setup!("$(if t)", parse, err);

    assert_err_contains!(parse, ParseErrorKind::InsufficientArguments("if"));
}

#[test]
fn four_args() {
    test_setup!("$(if t,a,b,c)", parse, err);

    assert_err_contains!(parse, ParseErrorKind::ExtraArguments("if"));
}

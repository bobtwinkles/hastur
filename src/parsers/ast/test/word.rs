//! tests for the word function

use super::*;
use pretty_assertions::assert_eq;

#[test]
fn basic() {
    let block = create_span("$(word 1,foo)");
    let res = assert_ok!(parse_ast(block.span()));
    assert_complete!(res.0);
    assert_eq!(
        res.1,
        ast::word(
            Location::test_location(1, 1),
            ast::constant(LocatedString::test_new(1, 8, "1")),
            ast::constant(LocatedString::test_new(1, 10, "foo"))
        )
    )
}

#[test]
fn too_few_args() {
    let block = create_span("$(word 1)");
    let err = assert_err!(parse_ast(block.span()));
    assert_err_contains!(err, ParseErrorKind::InsufficientArguments("word"));
}

#[test]
fn too_many_args() {
    let block = create_span("$(word 1,foo,extra)");
    let err = assert_err!(parse_ast(block.span()));
    assert_err_contains!(err, ParseErrorKind::ExtraArguments("word"));
}

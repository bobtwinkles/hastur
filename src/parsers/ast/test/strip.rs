//! Tests for the strip function
use super::*;
use pretty_assertions::assert_eq;

#[test]
fn basic() {
    let block = create_span("$(strip foo)");
    let res = assert_ok!(parse_ast(block.span()));
    assert_complete!(res.0);
    assert_eq!(
        res.1,
        ast::strip(
            Location::test_location(1, 1),
            ast::constant(LocatedString::test_new(1, 9, "foo"))
        )
    )
}

#[test]
fn too_many_args() {
    let block = create_span("$(strip foo,extra)");
    let err = assert_err!(parse_ast(block.span()));
    assert_err_contains!(err, ParseErrorKind::ExtraArguments("strip"));
}

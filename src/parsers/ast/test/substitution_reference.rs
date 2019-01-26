//! Tests for substitution references

use super::*;

#[test]
fn basic() {
    let block = create_span("$(FOO:.c=.o)");
    let res = assert_ok!(parse_ast(block.span()));
    assert_complete!(res.0);
    assert_eq!(
        res.1,
        ast::substitution_reference(
            Location::test_location(1, 1),
            ast::constant(
                Location::test_location(1, 3),
                LocatedString::test_new(1, 3, "foo")
            ),
            ast::constant(
                Location::test_location(1, 5),
                LocatedString::test_new(1, 5, ".c")
            ),
            ast::constant(
                Location::test_location(1, 8),
                LocatedString::test_new(1, 8, ".o")
            )
        ),
    )
}

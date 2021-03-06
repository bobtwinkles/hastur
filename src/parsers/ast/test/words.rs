//! Tests for the `words` function

use super::*;
use pretty_assertions::assert_eq;

#[test]
fn basic() {
    let block = create_span("$(words foo bar)");
    let res = assert_ok!(parse_ast(block.span()));
    assert_complete!(res.0);
    assert_eq!(
        res.1,
        ast::words(
            Location::test_location(1, 1),
            ast::constant(LocatedString::test_new(1, 9, "foo bar"))
        )
    )
}

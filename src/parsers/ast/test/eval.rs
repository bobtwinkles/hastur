//! Tests for eval

use super::*;
use pretty_assertions::assert_eq;

#[test]
fn basic() {
    test_setup!("$(eval 0)", parse, ok);
    assert_complete!(parse.0);
    assert_eq!(
        parse.1,
        ast::eval(
            Location::test_location(1, 1),
            ast::constant(LocatedString::test_new(1, 8, "0"))
        )
    )
}

#[test]
fn no_content() {
    // This behavior is demonstrated by running
    // make --warn-undefined-variables -f tests/makefiles/variable_named_eval.mk
    test_setup!("$(eval )", parse, ok);
    assert_complete!(parse.0);
    assert_eq!(
        parse.1,
        ast::eval(Location::test_location(1, 1), ast::empty())
    )
}

#[test]
fn really_no_content() {
    // This behavior is demonstrated by running
    // make --warn-undefined-variables -f tests/makefiles/variable_named_eval.mk
    test_setup!("$(eval)", parse, ok);
    assert_complete!(parse.0);
    assert_eq!(
        parse.1,
        ast::variable_reference(
            Location::test_location(1, 1),
            ast::constant(LocatedString::test_new(1, 3, "eval"))
        )
    )
}

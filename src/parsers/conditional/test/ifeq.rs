//! Tests for the ifeq parser

use super::*;
use crate::parsers::conditional::*;

// #TST-P-Conditional.ifeq_parens
#[test]
fn parens_simple() {
    let test_str = simple_line("(a, b)");

    let result = assert_ok!(parse_ifeq(test_str.span()));

    assert_complete!(result.0);
    assert_eq!(
        (result.1).0,
        ast::constant(LocatedString::test_new(1, 2, "a"))
    );
    assert_eq!(
        (result.1).1,
        ast::constant(LocatedString::test_new(1, 5, "b"))
    );
}

// #TST-P-Conditional.ifeq_nested_parens
#[test]
fn parens_nested() {
    let test_str = simple_line("($(foo_$(bar)), $(baz)_$(qux_$(big)))");

    let result = assert_ok!(parse_ifeq(test_str.span()));

    assert_complete!(result.0);
    assert_eq!(
        (result.1).0,
        ast::variable_reference(
            Location::test_location(1, 2),
            ast::collapsing_concat(
                Location::test_location(1, 4),
                vec![
                    ast::constant(LocatedString::test_new(1, 4, "foo_")),
                    ast::variable_reference(
                        Location::test_location(1, 8),
                        ast::constant(LocatedString::test_new(1, 10, "bar"))
                    )
                ]
            )
        )
    );
    assert_eq!(
        (result.1).1,
        ast::collapsing_concat(
            Location::test_location(1, 17),
            vec![
                ast::variable_reference(
                    Location::test_location(1, 17),
                    ast::constant(LocatedString::test_new(1, 19, "baz"))
                ),
                ast::constant(LocatedString::test_new(1, 23, "_")),
                ast::variable_reference(
                    Location::test_location(1, 24),
                    ast::collapsing_concat(
                        Location::test_location(1, 26),
                        vec![
                            ast::constant(LocatedString::test_new(1, 26, "qux_")),
                            ast::variable_reference(
                                Location::test_location(1, 30),
                                ast::constant(LocatedString::test_new(1, 32, "big"))
                            )
                        ]
                    )
                )
            ]
        )
    );
}

// #TST-P-Conditional.ifeq_dquote
#[test]
fn dquotes_simple() {
    let test_str = simple_line("\"a\" \"b\"");

    let result = assert_ok!(parse_ifeq(test_str.span()));
    assert_complete!(result.0);

    assert_eq!(
        (result.1).0,
        ast::constant(LocatedString::test_new(1, 2, "a"))
    );
    assert_eq!(
        (result.1).1,
        ast::constant(LocatedString::test_new(1, 6, "b"))
    );
}

// #TST-P-Conditional.ifeq_squote
#[test]
fn quotes_simple() {
    let test_str = simple_line("'a' 'b'");

    let result = assert_ok!(parse_ifeq(test_str.span()));

    assert_complete!(result.0);
    assert_eq!(
        (result.1).0,
        ast::constant(LocatedString::test_new(1, 2, "a"))
    );
    assert_eq!(
        (result.1).1,
        ast::constant(LocatedString::test_new(1, 6, "b"))
    );
}

// #TST-P-Conditional.ifeq_mixed_quote_sd
#[test]
fn quotes_dquotes_mixed() {
    let test_str = simple_line("'a' \"b\"");

    let result = assert_ok!(parse_ifeq(test_str.span()));

    assert_complete!(result.0);
    assert_eq!(
        (result.1).0,
        ast::constant(LocatedString::test_new(1, 2, "a"))
    );
    assert_eq!(
        (result.1).1,
        ast::constant(LocatedString::test_new(1, 6, "b"))
    );
}

// #TST-P-Conditional.ifeq_mixed_quote_ds
#[test]
fn dquotes_quotes_mixed() {
    let test_str = simple_line("\"a\" 'b'");

    let result = assert_ok!(parse_ifeq(test_str.span()));

    assert_complete!(result.0);
    assert_eq!(
        (result.1).0,
        ast::constant(LocatedString::test_new(1, 2, "a"))
    );
    assert_eq!(
        (result.1).1,
        ast::constant(LocatedString::test_new(1, 6, "b"))
    );
}

// #TST-P-Conditional.ifeq_err_bad_token
#[test]
fn bad_separator() {
    let test_str = simple_line("asdf");

    let result = parse_ifeq(test_str.span());
    assert!(result.is_err());
}

// TODO: more error condition testing

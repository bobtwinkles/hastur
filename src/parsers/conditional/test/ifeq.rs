//! Tests for the ifeq parser

use super::*;
use crate::parsers::conditional::*;

// #TST-P-Conditional.ifeq_parens
#[test]
fn parens_simple() {
    let test_str = simple_line("(a, b)");

    let result = parse_ifeq(test_str.span());
    eprintln!("{:?}", result);
    assert!(result.is_ok());
    let result = result.ok().unwrap();

    assert_eq!(result.0.len(), 0);
    assert_eq!((result.1).0.into_string(), String::from("a"));
    assert_eq!((result.1).1.into_string(), String::from("b"));
}

// #TST-P-Conditional.ifeq_nested_parens
#[test]
fn parens_nested() {
    let test_str = simple_line("($(foo_$(bar)), $(baz)_$(qux_$(big)))");

    let result = parse_ifeq(test_str.span());
    assert!(result.is_ok());
    let result = result.ok().unwrap();

    assert_eq!(result.0.len(), 0);
    assert_eq!((result.1).0.into_string(), String::from("$(foo_$(bar))"));
    assert_eq!(
        (result.1).1.into_string(),
        String::from("$(baz)_$(qux_$(big))")
    );
}

// #TST-P-Conditional.ifeq_dquote
#[test]
fn dquotes_simple() {
    let test_str = simple_line("\"a\" \"b\"");

    let result = parse_ifeq(test_str.span());
    assert!(result.is_ok());
    let result = result.ok().unwrap();

    assert_eq!(result.0.len(), 0);
    assert_eq!((result.1).0.into_string(), String::from("a"));
    assert_eq!((result.1).1.into_string(), String::from("b"));
}

// #TST-P-Conditional.ifeq_squote
#[test]
fn quotes_simple() {
    let test_str = simple_line("'a' 'b'");

    let result = parse_ifeq(test_str.span());
    assert!(result.is_ok());
    let result = result.ok().unwrap();

    assert_eq!(result.0.len(), 0);
    assert_eq!((result.1).0.into_string(), String::from("a"));
    assert_eq!((result.1).1.into_string(), String::from("b"));
}

// #TST-P-Conditional.ifeq_mixed_quote_sd
#[test]
fn quotes_dquotes_mixed() {
    let test_str = simple_line("'a' \"b\"");

    let result = parse_ifeq(test_str.span());
    assert!(result.is_ok());
    let result = result.ok().unwrap();

    assert_eq!(result.0.len(), 0);
    assert_eq!((result.1).0.into_string(), String::from("a"));
    assert_eq!((result.1).1.into_string(), String::from("b"));
}

// #TST-P-Conditional.ifeq_mixed_quote_ds
#[test]
fn dquotes_quotes_mixed() {
    let test_str = simple_line("\"a\" 'b'");

    let result = parse_ifeq(test_str.span());
    assert!(result.is_ok());
    let result = result.ok().unwrap();

    assert_eq!(result.0.len(), 0);
    assert_eq!((result.1).0.into_string(), String::from("a"));
    assert_eq!((result.1).1.into_string(), String::from("b"));
}

// #TST-P-Conditional.ifeq_err_bad_token
#[test]
fn bad_separator() {
    let test_str = simple_line("asdf");

    let result = parse_ifeq(test_str.span());
    assert!(result.is_err());
}

// TODO: more error condition testing

//! Testign for the AST parser

use super::*;
use crate::parsers::test::{create_span, error_context, error_list_contains};
use crate::source_location::{LocatedString, Location};

#[test]
fn simple() {
    let block = create_span("ab");
    let res = assert_ok!(parse_ast(block.span()));

    assert_complete!(res.0);
    assert_eq!(
        res.1,
        ast::constant(
            Location::test_location(1, 1),
            LocatedString::new(Location::test_location(1, 1).into(), "ab".into())
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
                LocatedString::new(Location::test_location(1, 2).into(), "a".into())
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
            LocatedString::new(Location::test_location(1, 1).into(), "$".into())
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
            LocatedString::new(Location::test_location(1, 2).into(), "$".into())
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
                LocatedString::new(Location::test_location(1, 3).into(), "foo".into())
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
                    LocatedString::new(Location::test_location(1, 5).into(), "foo".into())
                )
            )
        )
    );
}

#[test]
fn strip_basic() {
    let block = create_span("$(strip foo)");
    let res = assert_ok!(parse_ast(block.span()));
    assert_complete!(res.0);
    assert_eq!(
        res.1,
        ast::strip(
            Location::test_location(1, 3),
            ast::constant(
                Location::test_location(1, 9),
                LocatedString::new(Location::test_location(1, 9).into(), "foo".into())
            )
        )
    )
}

#[test]
fn strip_too_many_args() {
    let block = create_span("$(strip foo,extra)");
    let err = assert_err!(parse_ast(block.span()));
    assert_err_contains!(err, ParseErrorKind::ExtraArguments("strip"));
}

#[test]
fn word_basic() {
    let block = create_span("$(word 1,foo)");
    let res = assert_ok!(parse_ast(block.span()));
    assert_complete!(res.0);
    assert_eq!(
        res.1,
        ast::strip(
            Location::test_location(1, 3),
            ast::constant(
                Location::test_location(1, 9),
                LocatedString::new(Location::test_location(1, 9).into(), "foo".into())
            )
        )
    )
}

#[test]
fn argument_terminated_with_comma() {
    let block = create_span("foo,");
    let res = assert_ok!(function_argument(block.span()));

    assert_segments_eq!(res.0, [(",", Location::test_location(1, 4))]);
    assert_segments_eq!(res.1, [("foo", Location::test_location(1, 1))]);
}

#[test]
fn argument_terminated_with_eof() {
    let block = create_span("foo");
    let res = assert_ok!(function_argument(block.span()));

    assert_complete!(res.0);
    assert_segments_eq!(res.1, [("foo", Location::test_location(1, 1))]);
}

#[test]
fn argument_ignores_internal_commas() {
    let block = create_span("$(some_func a,b)");
    let res = assert_ok!(function_argument(block.span()));

    assert_complete!(res.0);
    assert_segments_eq!(res.1, [("$(some_func a,b)", Location::test_location(1, 1))]);

    let block = create_span("${some_func a,b}");
    let res = assert_ok!(function_argument(block.span()));

    assert_complete!(res.0);
    assert_segments_eq!(res.1, [("${some_func a,b}", Location::test_location(1, 1))]);
}

#[test]
fn unbalanced_reference() {
    let block = create_span("$(foo");
    let res = parse_ast(block.span());
    let err = std::dbg!(res).err().expect("AST should be an error");
    let context = error_context(err).expect("There should be an error context");
    let errors = nom::error_to_list(&context);
    assert!(error_list_contains(
        &errors,
        ErrorKind::Custom(ParseErrorKind::UnternimatedVariable)
    ));
}

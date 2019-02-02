//! Tests for the variable parsing infrastructure

// #TST-P-Variable

use super::*;
use crate::parsers::test::create_span;

mod define_keyword;

macro_rules! define_test (
    ($span:expr, $flavor:expr, $name:expr) => {{
        let database = Default::default();
        let mut names = Default::default();
        let block = create_span($span);
        let (remaining, (_database, variable_action)) = assert_ok!(parse_line(block.span(), &mut names, &database));
        assert_complete!(remaining);

        match variable_action.action {
            Action::Define(variable_data) => {
                assert_eq!(variable_data.flavor, $flavor);
            }
            e => panic!("Unexpected action {:?}", e),
        }

        assert_eq!(
            names.variable_name($name).expect("name was not interned"),
            variable_action.name
        );
    }}
);

// #TST-P-Variable.simple

// #TST-P-Variable.simple_assignment
#[test]
fn simple() {
    define_test!("a := b", Flavor::Simple, "a");
}

// #TST-P-Variable.simple_dcolon
#[test]
fn posix_simple() {
    define_test!("a ::= b", Flavor::Simple, "a");
}

// #TST-P-Variable.simple_nospace
#[test]
fn simple_no_space() {
    define_test!("a:= b", Flavor::Simple, "a");
}

// #TST-P-Variable.dcolon_nospace
#[test]
fn posix_simple_no_space() {
    define_test!("a::= b", Flavor::Simple, "a");
}

// #TST-P-Variable.recursive

// #TST-P-Variable.recursive_assignment
#[test]
fn recursive() {
    define_test!("a = b", Flavor::Recursive, "a")
}

// #TST-P-Variable.recursive_nospace
#[test]
fn recursive_no_space() {
    define_test!("a= b", Flavor::Recursive, "a")
}

#[test]
fn simple_modifiers() {
    let database = Default::default();
    let mut names = Default::default();
    let block = create_span("export a = b");
    let (remaining, (_database, variable_action)) =
        assert_ok!(parse_line(block.span(), &mut names, &database));
    assert_complete!(remaining);

    match variable_action.action {
        Action::Define(variable_data) => {
            assert_eq!(variable_data.flavor, Flavor::Recursive);
        }
        e => panic!("Unexpected action {:?}", e),
    }
    assert!(variable_action.modifiers.export);

    assert_eq!(
        names.variable_name("a").expect("name was not interned"),
        variable_action.name
    );
}

#[test]
fn modifers_with_no_var() {
    let database = Default::default();
    let mut names = Default::default();
    let block = create_span("export private");
    let err = assert_err!(parse_line(block.span(), &mut names, &database));

    assert_err_contains!(err, ParseErrorKind::InternalFailure("not an assignment"));
}

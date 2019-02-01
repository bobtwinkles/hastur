//! Tests for the variable parsing infrastructure

// #TST-P-Variable

use super::*;
use crate::parsers::test::create_span;

mod define_keyword;

// #TST-P-Variable.simple

// #TST-P-Variable.simple_assignment
#[test]
fn simple() {
    let mut database = Default::default();
    let block = create_span("a := b");
    let (remaining, variable_action) = assert_ok!(parse_line(block.span(), &mut database));
    assert_complete!(remaining);

    match variable_action.action {
        Action::Define(variable_data) => {
            assert_eq!(variable_data.flavor, Flavor::Simple);
        }
        e => panic!("Unexpected action {:?}", e),
    };

    assert_eq!(
        database.variable_name("a").expect("name was not interned"),
        variable_action.name
    );
}

// #TST-P-Variable.simple_dcolon
#[test]
fn posix_simple() {
    let mut database = Default::default();
    let block = create_span("a ::= b");
    let (remaining, variable_action) = assert_ok!(parse_line(block.span(), &mut database));
    assert_complete!(remaining);

    match variable_action.action {
        Action::Define(variable_data) => {
            assert_eq!(variable_data.flavor, Flavor::Simple);
        }
        e => panic!("Unexpected action {:?}", e),
    }

    assert_eq!(
        database.variable_name("a").expect("name was not interned"),
        variable_action.name
    );
}

// #TST-P-Variable.simple_nospace
#[test]
fn simple_no_space() {
    let mut database = Default::default();
    let block = create_span("a:= b");
    let (remaining, variable_action) = assert_ok!(parse_line(block.span(), &mut database));
    assert_complete!(remaining);

    match variable_action.action {
        Action::Define(variable_data) => {
            assert_eq!(variable_data.flavor, Flavor::Simple);
        }
        e => panic!("Unexpected action {:?}", e),
    }

    assert_eq!(
        database.variable_name("a").expect("name was not interned"),
        variable_action.name
    );
}

// #TST-P-Variable.dcolon_nospace
#[test]
fn posix_simple_no_space() {
    let mut database = Default::default();
    let block = create_span("a::= b");
    let (remaining, variable_action) = assert_ok!(parse_line(block.span(), &mut database));
    assert_complete!(remaining);

    match variable_action.action {
        Action::Define(variable_data) => {
            assert_eq!(variable_data.flavor, Flavor::Simple);
        }
        e => panic!("Unexpected action {:?}", e),
    }

    assert_eq!(
        database.variable_name("a").expect("name was not interned"),
        variable_action.name
    );
}

// #TST-P-Variable.recursive

// #TST-P-Variable.recursive_assignment
#[test]
fn recursive() {
    let mut database = Default::default();
    let block = create_span("a = b");
    let (remaining, variable_action) = assert_ok!(parse_line(block.span(), &mut database));
    assert_complete!(remaining);

    match variable_action.action {
        Action::Define(variable_data) => {
            assert_eq!(variable_data.flavor, Flavor::Recursive);
        }
        e => panic!("Unexpected action {:?}", e),
    }

    assert_eq!(
        database.variable_name("a").expect("name was not interned"),
        variable_action.name
    );
}

// #TST-P-Variable.recursive_nospace
#[test]
fn recursive_no_space() {
    let mut database = Default::default();
    let block = create_span("a= b");
    let (remaining, variable_action) = assert_ok!(parse_line(block.span(), &mut database));
    assert_complete!(remaining);

    match variable_action.action {
        Action::Define(variable_data) => {
            assert_eq!(variable_data.flavor, Flavor::Recursive);
        }
        e => panic!("Unexpected action {:?}", e),
    }

    assert_eq!(
        database.variable_name("a").expect("name was not interned"),
        variable_action.name
    );
}

#[test]
fn simple_modifiers() {
    let mut database = Default::default();
    let block = create_span("export a = b");
    let (remaining, variable_action) = assert_ok!(parse_line(block.span(), &mut database));
    assert_complete!(remaining);

    match variable_action.action {
        Action::Define(variable_data) => {
            assert_eq!(variable_data.flavor, Flavor::Recursive);
        }
        e => panic!("Unexpected action {:?}", e),
    }
    assert!(variable_action.modifiers.export);

    assert_eq!(
        database.variable_name("a").expect("name was not interned"),
        variable_action.name
    );
}

#[test]
fn modifers_with_no_var() {
    let mut database = Default::default();
    let block = create_span("export private");
    let err = assert_err!(parse_line(block.span(), &mut database));

    assert_err_contains!(err, ParseErrorKind::InternalFailure("not an assignment"));
}

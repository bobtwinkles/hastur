//! Tests for the variable parsing infrastructure

use super::*;
use crate::parsers::test::create_span;

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

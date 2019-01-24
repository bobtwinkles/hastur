//! Tests for the variable parsing infrastructure

use super::*;
use crate::parsers::test::create_span;

#[test]
fn simple() {
    let mut database = Default::default();
    let block = create_span("a := b");
    let (remaining, (variable_name, variable_data)) =
        assert_ok!(parse_line(block.span(), &mut database));

    assert_complete!(remaining);
    assert_eq!(variable_data.flavor, Flavor::Simple);
}

#[test]
fn recursive() {
    let mut database = Default::default();
    let block = create_span("a = b");
    let (remaining, (variable_name, variable_data)) =
        assert_ok!(parse_line(block.span(), &mut database));

    assert_complete!(remaining);
    assert_eq!(variable_data.flavor, Flavor::Recursive);
}

//! Tests for argument parsing

use super::*;

#[test]
fn terminated_with_comma() {
    let block = create_span("foo,");
    let res = assert_ok!(function_argument(block.span()));

    assert_segments_eq!(res.0, [(",", Location::test_location(1, 4))]);
    assert_segments_eq!(res.1, [("foo", Location::test_location(1, 1))]);
}

#[test]
fn terminated_with_eof() {
    let block = create_span("foo");
    let res = assert_ok!(function_argument(block.span()));

    assert_complete!(res.0);
    assert_segments_eq!(res.1, [("foo", Location::test_location(1, 1))]);
}

#[test]
fn ignores_internal_commas() {
    let block = create_span("$(some_func a,b)");
    let res = assert_ok!(function_argument(block.span()));

    assert_complete!(res.0);
    assert_segments_eq!(res.1, [("$(some_func a,b)", Location::test_location(1, 1))]);

    let block = create_span("${some_func a,b}");
    let res = assert_ok!(function_argument(block.span()));

    assert_complete!(res.0);
    assert_segments_eq!(res.1, [("${some_func a,b}", Location::test_location(1, 1))]);
}

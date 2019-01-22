use super::*;
use crate::evaluated::test::concat_node_with_locations;
#[macro_use]
use crate::evaluated::test;
use crate::source_location::Location;

/// #TST-P-Conditional.ifdef_simple
#[test]
fn simple() {
    let test_span = create_span("ifdef a");
    let test_span = test_span.span();
    let parse = assert_ok!(parse_line(test_span));

    assert_eq!(parse.0.segments().next(), None);
    assert_eq!(
        parse.1,
        Conditional::IfDef(concat_node_with_locations(&[(
            "a",
            Location::test_location(1, 7)
        )]))
    );
}

/// #TST-P-Conditional.ifndef_simple
#[test]
fn simple_ndef() {
    let test_span = create_span("ifndef a");
    let test_span = test_span.span();
    let parse = assert_ok!(parse_line(test_span));

    assert_eq!(parse.0.segments().next(), None);
    assert_eq!(
        parse.1,
        Conditional::IfNDef(concat_node_with_locations(&[(
            "a",
            Location::test_location(1, 8)
        )]))
    );
}

#[test]
fn multiple_whitespace() {
    let test_span = create_span("ifndef \t\\\n\t a");
    let test_span = test_span.span();
    let parse = assert_ok!(parse_line(test_span));

    assert_eq!(parse.0.segments().next(), None);
    assert_eq!(
        parse.1,
        Conditional::IfNDef(concat_node_with_locations(&[(
            "a",
            Location::test_location(2, 3)
        )]))
    );
}

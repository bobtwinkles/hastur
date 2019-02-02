use super::*;
use crate::source_location::Location;

/// #TST-P-Conditional.ifdef_simple
#[test]
fn simple() {
    let test_span = create_span("ifdef a");
    let test_span = test_span.span();
    let parse = assert_ok!(parse_line(test_span));
    assert_complete!(parse.0);
    match parse.1 {
        Conditional::IfDef(span) => {
            assert_segments_eq!(span.span(), [("a", Location::test_location(1, 7))])
        }
        v => panic!("Unexpected conditional type {:?}", v),
    }
}

/// #TST-P-Conditional.ifndef_simple
#[test]
fn simple_ndef() {
    let test_span = create_span("ifndef a");
    let test_span = test_span.span();
    let parse = assert_ok!(parse_line(test_span));
    assert_complete!(parse.0);
    match parse.1 {
        Conditional::IfNDef(span) => {
            assert_segments_eq!(span.span(), [("a", Location::test_location(1, 8))])
        }
        v => panic!("Unexpected conditional type {:?}", v),
    }
}

#[test]
fn multiple_whitespace() {
    let test_span = create_span("ifndef \t\\\n\t a");
    let test_span = assert_ok!(makefile_line(test_span.span(), ParserCompliance::GNU, true)).1;
    let parse = assert_ok!(parse_line(test_span.span()));
    assert_complete!(parse.0);
    match parse.1 {
        Conditional::IfNDef(span) => {
            assert_segments_eq!(span.span(), [("a", Location::test_location(2, 3))])
        }
        v => panic!("Unexpected conditional type {:?}", v),
    }
}

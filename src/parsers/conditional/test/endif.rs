//! Tests for endif
use super::*;
#[macro_use]
use crate::parsers::test;

#[test]
fn simple() {
    let line = create_span("endif");
    let line = line.span();
    let res = assert_ok!(parse_line(line));

    let mut segments_iter = res.0.segments();
    assert_eq!(segments_iter.next(), None);

    assert_eq!(res.1, Conditional::EndIf);
}

#[test]
fn extra_tokens() {
    let line = create_span("endif fdsa");
    let line = line.span();
    let res = parse_line(line);
    assert!(res.is_err());
    let errors = res.err().unwrap();
    let context = error_context(errors).unwrap();
    let errors = error_to_list(&context);
    assert!(error_list_contains(
        &errors,
        ErrorKind::Custom(ParseErrorKind::ExtraTokensAfter("endif"))
    ));
}

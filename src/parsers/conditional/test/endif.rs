//! Tests for endif
use super::*;

#[test]
fn simple() {
    let line = create_span("endif");
    let (_, line) = assert_ok!(makefile_line(line.span(), ParserCompliance::GNU, true));
    let res = assert_ok!(parse_line(line.span()));
    assert_complete!(res.0);
    assert_eq!(res.1, Conditional::EndIf);
}

#[test]
fn extra_tokens() {
    let line = create_span("endif fdsa");
    let (_, line) = assert_ok!(makefile_line(line.span(), ParserCompliance::GNU, true));
    let err = assert_err!(parse_line(line.span()));
    assert_err_contains!(err, ParseErrorKind::ExtraTokensAfter("endif"));
}

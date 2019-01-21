//! Tests for endif
use super::*;

#[test]
fn simple() {
    let line = create_span("endif");
    let line = line.span();
    let res = parse_line(line);

    assert_eq!(
        res,
        Ok((leftover_span("", 5, 1).span(), Conditional::EndIf))
    )
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

//! Tests for the else parser
use super::*;
use std::borrow::Borrow;

#[test]
fn simple() {
    let line = create_span("else");
    let line = line.span();
    let res = assert_ok!(parse_line(line));
    assert_complete!(res.0);
    assert_eq!(res.1, Conditional::Else(None));
}

/// #TST-P-Conditional.else_next_conditional
#[test]
fn simple_ifdef() {
    let line = create_span("else ifdef a");
    let line = line.span();
    let res = assert_ok!(parse_line(line));

    assert_complete!(res.0);
    match res.1 {
        Conditional::Else(Some(cond)) => match cond.borrow() {
            Conditional::IfDef(l) => assert_eq!(&l.into_string(), "a"),
            _ => panic!("Detected continuation was not an ifdef"),
        },
        _ => panic!("Failed to detect ifdef {:?}", res.1),
    }
}

/// #TST-P-Conditional.else_err_extra_tokens
#[test]
fn rejects_non_if() {
    let line = create_span("else asdf");
    let line = line.span();
    let res = parse_line(line);
    assert!(res.is_err());
}

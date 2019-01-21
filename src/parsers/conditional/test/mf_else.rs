//! Tests for the else parser
use super::*;
use std::borrow::Borrow;

#[test]
fn simple() {
    let line = create_span("else");
    let line = line.span();
    let res = parse_line(line);
    eprintln!("{:?}", res);
    assert!(res.is_ok());
    let res = res.ok().unwrap();

    assert_eq!(res.0, leftover_span("", 4, 1).span());
    assert_eq!(res.1, Conditional::Else(None));
}

/// #TST-P-Conditional.else_next_conditional
#[test]
fn simple_ifdef() {
    let line = create_span("else ifdef a");
    let line = line.span();
    let res = parse_line(line);
    assert!(res.is_ok());
    let res = res.ok().unwrap();

    assert_eq!(res.0, leftover_span("", 12, 1).span());
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

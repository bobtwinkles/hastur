// #TST-P-Conditional

use super::{parse_line, Conditional};
use crate::parsers::test::*;
use crate::ParseErrorKind;
use nom::{error_to_list, ErrorKind};

mod endif;
mod ifdef;
mod ifeq;
mod mf_else;

#[test]
fn rejects_non_keywords() {
    let test_span = create_span("this is not a conditional");
    let test_span = test_span.span();
    let parse = parse_line(test_span);

    assert!(parse.is_err());
    let parse = parse.err().unwrap();
    assert!(match parse {
        nom::Err::Error(_) => true,
        _ => false,
    });
    let context = error_context(parse);
    assert!(context.is_some());
    let context = context.unwrap();
    let errors = error_to_list(&context);
    eprintln!("{:?}", errors);
    assert!(error_list_contains(
        &errors,
        ErrorKind::Custom(ParseErrorKind::ConditionalExpected)
    ));
    // assert_eq!(parse, (leftover_span("a", 32, 2), ()));
}

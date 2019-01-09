use super::*;
use crate::parsers::CollapsedLine;

/// #TST-P-Conditional.ifdef_simple
#[test]
fn simple() {
    let test_span = create_span("ifdef a");
    let parse = parse_line(test_span);

    assert_eq!(
        parse,
        Ok((
            leftover_span("", 7, 1),
            Conditional::IfDef(CollapsedLine::new(vec![leftover_span("a", 6, 1)]))
        ))
    );
}

/// #TST-P-Conditional.ifndef_simple
#[test]
fn simple_ndef() {
    let test_span = create_span("ifndef a");
    let parse = parse_line(test_span);

    assert_eq!(
        parse,
        Ok((
            leftover_span("", 8, 1),
            Conditional::IfNDef(CollapsedLine::new(vec![leftover_span("a", 7, 1)]))
        ))
    );
}

#[test]
fn multiple_whitespace() {
    let test_span = create_span("ifndef \t\\\n\t a");
    let parse = parse_line(test_span);

    assert_eq!(
        parse,
        Ok((
            leftover_span("", 13, 2),
            Conditional::IfNDef(CollapsedLine::new(vec![leftover_span("a", 12, 2)]))
        ))
    );
}

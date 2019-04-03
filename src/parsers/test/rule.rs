//! Tests the rule parser
use super::create_span;
use crate::parsers::ParserState;
use crate::Engine;

#[test]
fn simple_rule() {
    let block = create_span(
        r#"
a: b
	echo bar
"#,
    );

    let mut engine: Engine = Default::default();
    let mut parse_state = ParserState::new("test");
    let mut names = Default::default();

    // Empty first line
    let (i, _) = assert_ok!(parse_state.parse_line(block.span(), &mut names, &mut engine));

    // rule line
    let (i, _) = assert_ok!(parse_state.parse_line(i, &mut names, &mut engine));

    // recipe line
    let (i, _) = assert_ok!(parse_state.parse_line(i, &mut names, &mut engine));

    // last line
    let (i, _) = assert_ok!(parse_state.parse_line(i, &mut names, &mut engine));

    assert_complete!(i);
}

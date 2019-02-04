//! Tests that exercise the full pipeline,
use super::create_span;
use crate::parsers::ParserState;
use crate::Engine;

#[test]
fn simple_conditional() {
    let block = create_span(
        r#"
foo := bar
ifeq ($(foo),bar)
    baz := correct
else
    baz := incorrect
endif
"#,
    );

    let mut engine: Engine = Default::default();
    let mut parse_state = ParserState::new("test");
    let mut names = Default::default();

    // Empty first line
    let (i, _) = assert_ok!(parse_state.parse_line(block.span(), &mut names, &mut engine));

    // Assignment line
    let (i, _) = assert_ok!(parse_state.parse_line(i, &mut names, &mut engine));
    // After processing the first line, foo should be interned
    let variable_name = names
        .variable_name("foo")
        .expect("Variable foo should have been interned");
    assert_eq!(
        engine
            .database
            .get_variable(variable_name)
            .expect("Variable foo should have been set")
            .expand(&mut names, &engine.database)
            .1,
        "bar"
    );

    // ifeq line
    let (i, _) = assert_ok!(parse_state.parse_line(i, &mut names, &mut engine));

    assert_eq!(
        parse_state.conditionals,
        vec![super::ConditionalState {
            interpretation: super::ConditionalInterpretation::Executing,
            seen_else: false
        }]
    );

    // correct assignment
    let (i, _) = assert_ok!(parse_state.parse_line(i, &mut names, &mut engine));

    // else line
    let (i, _) = assert_ok!(parse_state.parse_line(i, &mut names, &mut engine));

    assert_eq!(
        parse_state.conditionals,
        vec![super::ConditionalState {
            interpretation: super::ConditionalInterpretation::NotExecuting,
            seen_else: true
        }]
    );

    // Incorrect assignment
    let (i, _) = assert_ok!(parse_state.parse_line(i, &mut names, &mut engine));

    // endif line
    let (_i, _) = assert_ok!(parse_state.parse_line(i, &mut names, &mut engine));

    // At the end of everything, `baz` should be set to `correct`
    assert_eq!(parse_state.conditionals, vec![]);
    let variable_name = names
        .variable_name("baz")
        .expect("Variable baz should have been interned");
    assert_eq!(
        engine
            .database
            .get_variable(variable_name)
            .expect("Variable baz should have been set")
            .expand(&mut names, &engine.database)
            .1,
        "correct"
    );
}

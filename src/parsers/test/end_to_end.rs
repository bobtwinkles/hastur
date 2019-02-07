//! Tests that exercise the full pipeline,
use super::create_span;
use crate::parsers::ParserState;
use crate::Engine;

macro_rules! variable_set_to (
    ($names:expr, $engine:expr, $variable_name:expr, $value:expr) => {{
        let names = &mut $names;
        let engine = &$engine;
        let variable_name = $variable_name;
        let value = $value;

        let variable_name = names
            .variable_name(variable_name)
            .expect(&format!(
                "Variable named {:?} should have been interned",
                variable_name
            ));

        assert_eq!(
            &engine
                .database
                .get_variable(variable_name)
                .expect(&format!("Variable named {:?} should have a value", variable_name))
                .expand(names, &engine.database).1,
            value
        )
    }}
);

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
    variable_set_to!(names, engine, "foo", "bar");

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
    variable_set_to!(names, engine, "baz", "correct");
}

#[test]
fn simple_append() {
    let block = create_span(
        r#"
foo := bar
foo += baz
"#,
    );

    let mut engine: Engine = Default::default();
    let mut parse_state = ParserState::new("test");
    let mut names = Default::default();

    // Empty first line
    let (i, _) = assert_ok!(parse_state.parse_line(block.span(), &mut names, &mut engine));
    // First set
    let (i, _) = assert_ok!(parse_state.parse_line(i, &mut names, &mut engine));
    variable_set_to!(names, engine, "foo", "bar");

    // Second set
    let (_i, _) = assert_ok!(parse_state.parse_line(i, &mut names, &mut engine));
    variable_set_to!(names, engine, "foo", "bar baz");
}

#[test]
fn simple_multiline_define() {
    let block = create_span(
        r#"
define foo
  bar \\
  baz
endef
"#,
    );

    let mut engine: Engine = Default::default();
    let mut parse_state = ParserState::new("test");
    let mut names = Default::default();

    // Empty first line
    let (i, _) = assert_ok!(parse_state.parse_line(block.span(), &mut names, &mut engine));
    // Define line
    let (i, _) = assert_ok!(parse_state.parse_line(i, &mut names, &mut engine));
    // add bar
    let (i, _) = assert_ok!(parse_state.parse_line(i, &mut names, &mut engine));
    // add baz
    let (i, _) = assert_ok!(parse_state.parse_line(i, &mut names, &mut engine));

    // End of define line
    let (_i, _) = assert_ok!(parse_state.parse_line(i, &mut names, &mut engine));
    variable_set_to!(names, engine, "foo", "  bar \\\\\n  baz\n");
}

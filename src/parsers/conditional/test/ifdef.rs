use super::*;

use crate::parsers::{ConditionalInterpretation, ConditionalState, ParserState};
use crate::{Engine, NameCache};

macro_rules! run_line_parse_for {
    ($content:expr) => {{
        crate::test::setup();

        let test_span = create_span($content);
        let test_span = test_span.span();
        let parse = assert_ok!(parse_line(test_span));

        assert_complete!(parse.0);

        parse.1
    }};
}

/// #TST-P-Conditional.ifdef_simple
#[test]
fn simple() {
    let parse = run_line_parse_for!("ifdef a");

    match parse.clone() {
        Conditional::IfDef(var) => {
            assert_eq!(var, ast::constant(LocatedString::test_new(1, 7, "a")))
        }
        v => panic!("Unexpected conditional type {:?}", v),
    }

    let mut engine: Engine = Default::default();
    let mut parse_state = ParserState::new();
    let mut names = Default::default();

    let _ = parse_state
        .handle_conditional(parse, &mut names, &mut engine)
        .unwrap();
    assert!(parse_state.ignoring);
    assert_eq!(
        parse_state.conditionals,
        vec![ConditionalState {
            interpretation: ConditionalInterpretation::NotExecuting,
            seen_else: false
        }]
    );
}

/// #TST-P-Conditional.ifndef_simple
#[test]
fn simple_ndef() {
    let parse = run_line_parse_for!("ifndef a");

    match parse.clone() {
        Conditional::IfNDef(var) => {
            assert_eq!(var, ast::constant(LocatedString::test_new(1, 8, "a")))
        }
        v => panic!("Unexpected conditional type {:?}", v),
    }

    let mut engine: Engine = Default::default();
    let mut parse_state = ParserState::new();
    let mut names = Default::default();

    let _ = parse_state
        .handle_conditional(parse, &mut names, &mut engine)
        .unwrap();
    assert!(!parse_state.ignoring);
    assert_eq!(
        parse_state.conditionals,
        vec![ConditionalState {
            interpretation: ConditionalInterpretation::Executing,
            seen_else: false
        }]
    );
}

#[test]
fn ndef_fail() {
    let parse = run_line_parse_for!("ifndef a");

    match parse.clone() {
        Conditional::IfNDef(var) => {
            assert_eq!(var, ast::constant(LocatedString::test_new(1, 8, "a")))
        }
        v => panic!("Unexpected conditional type {:?}", v),
    }

    let mut engine: Engine = Default::default();
    let mut parse_state = ParserState::new();
    let mut names = NameCache::default();

    let vname = names.intern_variable_name("a".into());
    engine.database = engine.database.set_variable(
        vname,
        crate::VariableParameters {
            unexpanded_value: crate::ast::empty(),
            flavor: crate::Flavor::Simple,
            origin: crate::Origin::File,
        },
    );

    let _ = parse_state
        .handle_conditional(parse, &mut names, &mut engine)
        .unwrap();
    assert!(parse_state.ignoring);
    assert_eq!(
        parse_state.conditionals,
        vec![ConditionalState {
            interpretation: ConditionalInterpretation::NotExecuting,
            seen_else: false
        }]
    );
}

#[test]
fn multiple_whitespace() {
    let test_span = create_span("ifndef \t\\\n\t a");
    let test_span = assert_ok!(makefile_line(test_span.span(), ParserCompliance::GNU, true)).1;
    let parse = assert_ok!(parse_line(test_span.span()));
    assert_complete!(parse.0);
    match parse.1 {
        Conditional::IfNDef(var) => {
            assert_eq!(var, ast::constant(LocatedString::test_new(2, 3, "a")))
        }
        v => panic!("Unexpected conditional type {:?}", v),
    }
}

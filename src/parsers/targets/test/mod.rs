//! Testing for target parsing

use super::*;
use crate::ast;
use crate::parsers::test::{create_span, leftover_span, separated_spans};
use crate::source_location::{LocatedString, Location};
use crate::NameCache;
use pretty_assertions::assert_eq;

mod proptest;

macro_rules! simple_test (
    ($v:expr, $e:expr) => {{
        let block = create_span($v);
        let res = assert_ok!(next_mword(block.span()));

        assert_complete!(res.0);
        assert_eq!((res.1).1, $e);
    }}
);

macro_rules! test_leaving(
    ($v:expr, $e:expr, $l:expr) => {{
        let init = $v;
        let fin = $l;
        let block = create_span(init);
        let res = assert_ok!(next_mword(block.span()));

        assert_segments_eq!(res.0, [(fin, Location::test_location(1, 1 + (init.len() - fin.len()) as u32))]);
        assert_eq!((res.1).1, $e);
    }}
);

#[test]
fn tricky_colon_escapes() {
    crate::test::setup();
    let block = create_span(r"\\:\:");
    let mut name_cache = NameCache::default();
    let t1 = leftover_span(r"\", 1, 1);
    let d1 = leftover_span(r":", 5, 1);
    let mut engine = Default::default();

    let (_, action) = assert_ok!(parse_line(block.span(), &mut name_cache, &mut engine));

    assert_eq!(
        action,
        Action::NewRule {
            targets: vec![t1],
            deps: vec![d1],
            double_colon: false,
            initial_command: None,
        }
    )
}

#[test]
fn backslash_in_target() {
    let block = create_span("\\\\ : \\\\\\;");
    let mut name_cache = NameCache::default();
    let t1 = leftover_span("\\", 1, 1);
    let d1 = leftover_span(";", 9, 1);
    let mut engine = Default::default();

    let (_, action) = assert_ok!(parse_line(block.span(), &mut name_cache, &mut engine));

    assert_eq!(
        action,
        Action::NewRule {
            targets: vec![t1],
            deps: vec![d1],
            double_colon: false,
            initial_command: None,
        }
    )
}

#[test]
fn backslash_at_end_of_input() {
    crate::test::setup();

    let block = create_span(r"\\ : \\");
    let mut name_cache = NameCache::default();
    let t1 = leftover_span(r"\", 1, 1);
    let d1 = leftover_span(r"\", 6, 1);
    let mut engine = Default::default();

    let (_, action) = assert_ok!(parse_line(block.span(), &mut name_cache, &mut engine));

    assert_eq!(
        action,
        Action::NewRule {
            targets: vec![t1],
            deps: vec![d1],
            double_colon: false,
            initial_command: None,
        }
    )
}

#[test]
fn two_backslash_targets() {
    let block = create_span(r"\\ \\ : \\\;");
    let mut name_cache = NameCache::default();
    let t1 = leftover_span(r"\", 1, 1);
    let t2 = leftover_span(r"\", 4, 1);
    let d = leftover_span(";", 12, 1);
    let mut engine = Default::default();

    let (_, action) = assert_ok!(parse_line(block.span(), &mut name_cache, &mut engine));

    assert_eq!(
        action,
        Action::NewRule {
            targets: vec![t1, t2],
            deps: vec![d],
            double_colon: false,
            initial_command: None,
        }
    )
}

#[test]
fn semicolon_as_target_with_command() {
    crate::test::setup();

    let block = create_span(r"\\\; : a;b");
    let mut name_cache = NameCache::default();
    let t = separated_spans(&[(1, 1, "\\"), (1, 4, ";")]);
    let d = leftover_span("a", 8, 1);
    let mut engine = Default::default();

    let (_, action) = assert_ok!(parse_line(block.span(), &mut name_cache, &mut engine));

    assert_eq!(
        action,
        Action::NewRule {
            targets: vec![t],
            deps: vec![d],
            double_colon: false,
            initial_command: Some(ast::constant(LocatedString::test_new(1, 10, "b"))),
        }
    )
}

#[test]
fn colon_as_target_with_command() {
    crate::test::setup();

    let block = create_span(r"\: : \\\;;b");
    let mut name_cache = NameCache::default();
    let t = leftover_span(":", 2, 1);
    let d = separated_spans(&[(1, 6, "\\"), (1, 9, ";")]);
    let mut engine = Default::default();

    let (_, action) = assert_ok!(parse_line(block.span(), &mut name_cache, &mut engine));

    assert_eq!(
        action,
        Action::NewRule {
            targets: vec![t],
            deps: vec![d],
            double_colon: false,
            initial_command: Some(ast::constant(LocatedString::test_new(1, 11, "b"))),
        }
    )
}

#[test]
fn mixed_semicolon_target() {
    crate::test::setup();

    let block = create_span(r"\\\; a :: a");
    let mut name_cache = NameCache::default();
    let t1 = separated_spans(&[(1, 1, "\\"), (1, 4, ";")]);
    let t2 = leftover_span("a", 6, 1);
    let d = leftover_span("a", 11, 1);
    let mut engine = Default::default();

    let (_, action) = assert_ok!(parse_line(block.span(), &mut name_cache, &mut engine));

    assert_eq!(
        action,
        Action::NewRule {
            targets: vec![t1, t2],
            deps: vec![d],
            double_colon: true,
            initial_command: None,
        }
    )
}

#[test]
fn command_simple() {
    let block = create_span("a : a;b");
    let mut name_cache = NameCache::default();
    let t = leftover_span("a", 1, 1);
    let d = leftover_span("a", 5, 1);
    let mut engine = Default::default();

    let (_, action) = assert_ok!(parse_line(block.span(), &mut name_cache, &mut engine));

    assert_eq!(
        action,
        Action::NewRule {
            targets: vec![t],
            deps: vec![d],
            double_colon: false,
            initial_command: Some(ast::constant(LocatedString::test_new(1, 7, "b"))),
        }
    )
}

#[test]
fn command_dcolon() {
    let block = create_span("a :: a;b");
    let mut name_cache = NameCache::default();
    let t = leftover_span("a", 1, 1);
    let d = leftover_span("a", 6, 1);
    let mut engine = Default::default();

    let (_, action) = assert_ok!(parse_line(block.span(), &mut name_cache, &mut engine));

    assert_eq!(
        action,
        Action::NewRule {
            targets: vec![t],
            deps: vec![d],
            double_colon: true,
            initial_command: Some(ast::constant(LocatedString::test_new(1, 8, "b"))),
        }
    )
}

#[test]
fn escaped_colon_target() {
    crate::test::setup();
    let block = create_span("\\: :: a");
    let mut name_cache = NameCache::default();
    let t = leftover_span(":", 2, 1);
    let d = leftover_span("a", 7, 1);
    let mut engine = Default::default();

    let (_, action) = assert_ok!(parse_line(block.span(), &mut name_cache, &mut engine));

    assert_eq!(
        action,
        Action::NewRule {
            targets: vec![t],
            deps: vec![d],
            double_colon: true,
            initial_command: None,
        }
    )
}

#[test]
fn escaped_semicolon_target() {
    crate::test::setup();
    let block = create_span(r"\\\; : a");
    let mut name_cache = NameCache::default();
    let t = separated_spans(&[(1, 1, "\\"), (1, 4, ";")]);
    let d = leftover_span("a", 8, 1);
    let mut engine = Default::default();

    let (_, action) = assert_ok!(parse_line(block.span(), &mut name_cache, &mut engine));

    assert_eq!(
        action,
        Action::NewRule {
            targets: vec![t],
            deps: vec![d],
            double_colon: false,
            initial_command: None,
        }
    )
}

#[test]
fn test_mword_early_outs() {
    simple_test!(":", MWordEnd::Colon);
    simple_test!("=", MWordEnd::VarAssign);
    simple_test!(":=", MWordEnd::VarAssign);
    simple_test!("::=", MWordEnd::VarAssign);
    simple_test!("::", MWordEnd::DColon);
    simple_test!(":", MWordEnd::Colon);
    simple_test!("+=", MWordEnd::VarAssign);
    simple_test!("?=", MWordEnd::VarAssign);
    simple_test!("!=", MWordEnd::VarAssign);
}

#[test]
fn test_simple_var_expansions() {
    simple_test!("$$", MWordEnd::Static);
    simple_test!("$$a", MWordEnd::Static);
    simple_test!("$a", MWordEnd::Variable);
    simple_test!("$(a)", MWordEnd::Variable);
    simple_test!("${a}", MWordEnd::Variable);
}

#[test]
fn leaves_assign() {
    test_leaving!("a=", MWordEnd::Static, "=");
    test_leaving!("$$=", MWordEnd::Static, "=");
    test_leaving!("a?=", MWordEnd::Static, "?=");
    test_leaving!("$(a)?=", MWordEnd::Variable, "?=");
    test_leaving!("a+=", MWordEnd::Static, "+=");
    test_leaving!("$(a)+=", MWordEnd::Variable, "+=");
}

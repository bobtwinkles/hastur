//! Testing for target parsing

use super::*;
use crate::parsers::test::create_span;
use crate::source_location::{LocatedString, Location};

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

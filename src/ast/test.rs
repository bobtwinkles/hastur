//! Tests for AST evaluation

use super::*;
use crate::eval::Flavor;
use crate::evaluated;
use crate::evaluated::test::single_block;
use crate::parsers::ast::parse_ast;
use crate::parsers::variable::parse_line as parse_variable_line;
use crate::source_location::{LocatedString, Location};
use crate::test::empty_database;
use crate::Database;

macro_rules! ast_parse {
    ($block:expr) => {{
        let res = assert_ok!(parse_ast(($block).span()));
        assert_complete!(res.0);
        res.1
    }};
}

/// Simplistic wrapper that creates a block from a single content reference,
/// with empty sensitivity
fn block_from_reference(c: ContentReference) -> std::sync::Arc<evaluated::Block> {
    Block::new(Default::default(), vec![c])
}

fn insert_variable_from_line(db: &mut Database, variable: LocatedString) -> VariableName {
    let block = block_from_reference(evaluated::constant(variable));
    let variable_op = assert_ok!(parse_variable_line(block.span(), db));
    assert_complete!(variable_op.0);
    let (variable_name, variable_op) = variable_op.1;

    match variable_op.flavor {
        Flavor::Recursive | Flavor::Simple => db.set_variable(variable_name, variable_op),
        _ => unimplemented!(),
    }

    variable_name
}

#[test]
fn const_sanity() {
    let block = single_block("foo");
    let ast = ast_parse!(block);
    let mut database = empty_database();
    let evaluated = ast.eval(&mut database);
    assert_eq!(
        evaluated,
        block_from_reference(evaluated::constant(LocatedString::test_new(1, 1, "foo")))
    )
}

#[test]
fn var_ref_simple() {
    let block = single_block("$(foo)");
    let ast = ast_parse!(block);

    let mut database = empty_database();
    let variable =
        insert_variable_from_line(&mut database, LocatedString::test_new(2, 1, "foo := bar"));
    let expected_sensitivity = [variable].iter().map(|x| *x).collect();

    let val = ast.eval(&mut database);
    assert_eq!(
        val,
        Block::new(
            expected_sensitivity,
            vec![evaluated::variable_reference(
                block_from_reference(evaluated::constant(LocatedString::test_new(1, 3, "foo"))),
                block_from_reference(evaluated::constant(LocatedString::test_new(2, 8, "bar")))
            )]
        )
    )
}

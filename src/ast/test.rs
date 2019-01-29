//! Tests for AST evaluation

use super::*;
use crate::eval::Flavor;
use crate::evaluated;
use crate::evaluated::test::single_block;
use crate::parsers::ast::parse_ast;
use crate::parsers::variable::parse_line as parse_variable_line;
use crate::source_location::LocatedString;
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
    use crate::parsers::variable::Action;

    let block = block_from_reference(evaluated::constant(variable));
    let variable_op = assert_ok!(parse_variable_line(block.span(), db));
    assert_complete!(variable_op.0);
    let variable_op = variable_op.1;

    match variable_op.action {
        Action::Define(params) => match params.flavor {
            Flavor::Recursive | Flavor::Simple => db.set_variable(variable_op.name, params),
            v => unimplemented!("{:?}", v),
        },
        v => unimplemented!("{:?}", v),
    }

    variable_op.name
}

fn mk_sensitivity(expected: &[VariableName]) -> fxhash::FxHashSet<VariableName> {
    expected.iter().map(|x| *x).collect()
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
    let expected_sensitivity = mk_sensitivity(&[variable]);

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

#[test]
fn var_ref_substitution() {
    let block = single_block("$(foo:.c=.o)");
    let ast = ast_parse!(block);

    let mut database = empty_database();
    let variable =
        insert_variable_from_line(&mut database, LocatedString::test_new(2, 1, "foo := a.c"));
    let expected_sensitivity = mk_sensitivity(&[variable]);

    let val = ast.eval(&mut database);
    assert_eq!(
        val,
        Block::new(
            expected_sensitivity.clone(),
            vec![evaluated::substitution_reference(
                block_from_reference(evaluated::constant(LocatedString::test_new(1, 3, "foo"))),
                block_from_reference(evaluated::constant(LocatedString::test_new(1, 7, ".c"))),
                block_from_reference(evaluated::constant(LocatedString::test_new(1, 9, ".o"))),
                Block::new(
                    expected_sensitivity,
                    vec![
                        evaluated::constant(LocatedString::test_new(2, 8, "a")),
                        evaluated::constant(LocatedString::test_new(1, 9, ".o")),
                    ]
                )
            )]
        )
    )
}

#[test]
fn var_ref_substitution_complex() {
    let block = single_block("$(foo:.c==.o)");
    let ast = ast_parse!(block);

    let mut database = empty_database();
    let variable =
        insert_variable_from_line(&mut database, LocatedString::test_new(2, 1, "foo := a.c="));
    let expected_sensitivity = mk_sensitivity(&[variable]);

    let val = ast.eval(&mut database);
    assert_eq!(
        val,
        Block::new(
            expected_sensitivity.clone(),
            vec![evaluated::substitution_reference(
                block_from_reference(evaluated::constant(LocatedString::test_new(1, 3, "foo"))),
                block_from_reference(evaluated::constant(LocatedString::test_new(1, 7, ".c="))),
                block_from_reference(evaluated::constant(LocatedString::test_new(1, 9, ".o"))),
                Block::new(
                    expected_sensitivity,
                    vec![
                        evaluated::constant(LocatedString::test_new(2, 8, "a")),
                        evaluated::constant(LocatedString::test_new(1, 9, ".o")),
                    ]
                )
            )]
        )
    )
}

#[test]
fn var_ref_recursive() {
    let block = single_block("$($(foo))");
    let ast = ast_parse!(block);

    let mut database = empty_database();
    let foo = insert_variable_from_line(&mut database, LocatedString::test_new(2, 1, "foo := bar"));
    let bar = insert_variable_from_line(&mut database, LocatedString::test_new(3, 1, "bar := baz"));

    let foo_senstivity = mk_sensitivity(&[foo]);
    let overall_sensitivity = mk_sensitivity(&[foo, bar]);

    let val = ast.eval(&mut database);

    assert_eq!(
        val,
        Block::new(
            overall_sensitivity,
            vec![evaluated::variable_reference(
                Block::new(
                    foo_senstivity,
                    vec![evaluated::variable_reference(
                        block_from_reference(evaluated::constant(LocatedString::test_new(
                            1, 5, "foo"
                        ))),
                        block_from_reference(evaluated::constant(LocatedString::test_new(
                            2, 8, "bar"
                        ))),
                    )]
                ),
                block_from_reference(evaluated::constant(LocatedString::test_new(3, 8, "baz")))
            )]
        )
    )
}

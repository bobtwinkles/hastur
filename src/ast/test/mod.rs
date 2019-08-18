//! Tests for AST evaluation

use super::*;
use crate::eval::Flavor;
use crate::evaluated;
use crate::evaluated::test::single_block;
use crate::parsers::ast::parse_ast;
use crate::parsers::variable::parse_line as parse_variable_line;
use crate::source_location::LocatedString;
use crate::NameCache;

macro_rules! ast_parse {
    ($block:expr) => {{
        let res = assert_ok!(parse_ast(($block).span()));
        assert_complete!(res.0);
        debug!("Parsed AST {:?}", res.1);
        res.1
    }};
}

macro_rules! ast_test_setup {
    ($engine:ident, $names:ident, $ast:ident, $block:expr) => {
        crate::test::setup();

        let block = single_block($block);
        let $ast = ast_parse!(block);

        let mut $engine = Engine::default();
        let mut $names = Default::default();
    }
}

mod abspath;
mod strip;
mod var_ref;
mod firstword;
mod findstring;

/// Simplistic wrapper that creates a block from a single content reference,
/// with empty sensitivity
fn block_from_reference(c: ContentReference) -> std::sync::Arc<evaluated::Block> {
    Block::new(Default::default(), vec![c])
}

fn insert_variable_from_line(
    names: &mut NameCache,
    context: &mut Engine,
    variable: LocatedString,
) -> VariableName {
    use crate::parsers::variable::Action;

    let block = block_from_reference(evaluated::constant(variable));
    let (remaining, variable_op) = assert_ok!(parse_variable_line(block.span(), names, context));
    assert_complete!(remaining);

    match variable_op.action {
        Action::Define(params) => match params.flavor {
            Flavor::Recursive | Flavor::Simple => {
                context.replace_database(context.database.set_variable(variable_op.name, params))
            }
            v => unimplemented!("{:?}", v),
        },
        v => unimplemented!("{:?}", v),
    }

    variable_op.name
}

fn mk_sensitivity(expected: &[VariableName]) -> crate::types::Set<VariableName> {
    expected.iter().map(|x| *x).collect()
}

#[test]
fn const_sanity() {
    let block = single_block("foo");
    let ast = ast_parse!(block);
    let mut database = Default::default();
    let mut names = Default::default();
    let evaluated = ast.eval(&mut names, &mut database);
    assert_eq!(
        evaluated,
        block_from_reference(evaluated::constant(LocatedString::test_new(1, 1, "foo")))
    )
}

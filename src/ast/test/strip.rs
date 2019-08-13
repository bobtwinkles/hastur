//! Tests for the strip function

use super::*;
use pretty_assertions::assert_eq;

#[test]
fn simple() {
    crate::test::setup();

    let block = single_block("$(strip foo)");
    let ast = ast_parse!(block);

    let mut engine = Engine::default();
    let mut names = Default::default();

    let evaluated = ast.eval(&mut names, &mut engine);

    assert_eq!(
        evaluated,
        block_from_reference(evaluated::strip(
            block_from_reference(evaluated::constant(LocatedString::test_new(1, 9, "foo"))),
            block_from_reference(evaluated::concat(
                Default::default(),
                vec![evaluated::constant(
                    LocatedString::test_new(1, 9, "foo")
                )]
            ))
        ))
    );
}

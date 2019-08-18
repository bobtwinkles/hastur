//! Findstring end to end testing

use super::*;
use pretty_assertions::assert_eq;

#[test]
fn simple() {
    crate::test::setup();

    let block = single_block("$(findstring bar,foobar)");
    let ast = ast_parse!(block);

    let mut engine = Engine::default();
    let mut names = Default::default();

    let evaluated = ast.eval(&mut names, &mut engine);

    assert_eq!(
        evaluated,
        block_from_reference(evaluated::findstring(
            block_from_reference(evaluated::constant(LocatedString::test_new(1, 14, "bar"))),
            block_from_reference(evaluated::constant(LocatedString::test_new(
                1, 18, "foobar"
            ))),
            block_from_reference(evaluated::concat(
                Default::default(),
                vec![evaluated::constant(LocatedString::test_new(1, 21, "bar"))]
            ))
        ))
    );
}

#[test]
fn notfound() {
    crate::test::setup();

    let block = single_block("$(findstring baz,foobar)");
    let ast = ast_parse!(block);

    let mut engine = Engine::default();
    let mut names = Default::default();

    let evaluated = ast.eval(&mut names, &mut engine);

    assert_eq!(
        evaluated,
        block_from_reference(evaluated::findstring(
            block_from_reference(evaluated::constant(LocatedString::test_new(1, 14, "baz"))),
            block_from_reference(evaluated::constant(LocatedString::test_new(
                1, 18, "foobar"
            ))),
            block_from_reference(evaluated::concat(Default::default(), vec![]))
        ))
    );
}

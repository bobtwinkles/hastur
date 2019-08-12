//! Tests for the abspath

use super::*;
use pretty_assertions::assert_eq;

#[test]
fn simple() {
    crate::test::setup();

    let block = single_block("$(abspath bar)");
    let ast = ast_parse!(block);

    let mut engine = Engine::default();
    let mut names = Default::default();

    engine.working_directory = "/foo".into();

    let evaluated = ast.eval(&mut names, &mut engine);

    assert_eq!(
        evaluated,
        block_from_reference(evaluated::abspath(
            block_from_reference(evaluated::constant(LocatedString::test_new(1, 11, "bar"))),
            Block::new(
                Default::default(),
                vec![
                    evaluated::constant(LocatedString::synthetic_new("/foo/")),
                    evaluated::concat(
                        Default::default(),
                        vec![evaluated::constant(LocatedString::test_new(1, 11, "bar"))]
                    )
                ]
            )
        ))
    )
}

#[test]
fn two_refs() {
    crate::test::setup();

    let block = single_block("$(abspath bar baz)");
    let ast = ast_parse!(block);

    let mut engine = Engine::default();
    let mut names = Default::default();

    engine.working_directory = "/foo".into();

    let evaluated = ast.eval(&mut names, &mut engine);

    assert_eq!(
        evaluated,
        block_from_reference(evaluated::abspath(
            block_from_reference(evaluated::constant(LocatedString::test_new(
                1, 11, "bar baz"
            ))),
            Block::new(
                Default::default(),
                vec![
                    evaluated::constant(LocatedString::synthetic_new("/foo/")),
                    evaluated::concat(
                        Default::default(),
                        vec![evaluated::constant(LocatedString::test_new(1, 11, "bar"))],
                    ),
                    evaluated::constant(LocatedString::synthetic_new(" ")),
                    evaluated::constant(LocatedString::synthetic_new("/foo/")),
                    evaluated::concat(
                        Default::default(),
                        vec![evaluated::constant(LocatedString::test_new(1, 15, "baz"))],
                    )
                ]
            )
        ))
    )
}

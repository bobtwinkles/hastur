//! Tests for the firstword function

use super::*;
use pretty_assertions::assert_eq;

#[test]
fn simple() {
    ast_test_setup!(engine, names, ast, "$(firstword foo bar)");

    let evaluated = ast.eval(&mut names, &mut engine);

    assert_eq!(
        evaluated,
        block_from_reference(evaluated::firstword(
            block_from_reference(evaluated::constant(LocatedString::test_new(
                1, 13, "foo bar"
            ))),
            Block::new(
                Default::default(),
                vec![evaluated::concat(
                    Default::default(),
                    vec![evaluated::constant(LocatedString::test_new(1, 13, "foo"))]
                )]
            )
        ))
    );
}

#[test]
fn tracks_sensitivity() {
    ast_test_setup!(engine, names, ast, "$(firstword foo $(BAR))");

    let evaluated = ast.eval(&mut names, &mut engine);

    let sens: Set<_> = [names.variable_name("BAR").unwrap()]
        .iter()
        .map(|x| *x)
        .collect();

    assert_eq!(
        evaluated,
        Block::new(
            sens.clone(),
            vec![evaluated::firstword(
                Block::new(
                    sens.clone(),
                    vec![
                        evaluated::constant(LocatedString::test_new(1, 13, "foo ")),
                        evaluated::variable_reference(
                            block_from_reference(evaluated::constant(LocatedString::test_new(
                                1, 18, "BAR"
                            ))),
                            Block::new(Default::default(), vec![])
                        )
                    ]
                ),
                Block::new(
                    // TODO: Sensitivity tracking should be more precise and drop
                    // the BAR, since there's no way it could impact this text
                    sens.clone(),
                    vec![evaluated::concat(
                        sens,
                        vec![evaluated::constant(LocatedString::test_new(1, 13, "foo"))]
                    )]
                )
            )]
        )
    );
}

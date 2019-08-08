//! Tests for variable references in ASTs

use super::*;

#[test]
fn simple() {
    let block = single_block("$(foo)");
    let ast = ast_parse!(block);

    let mut engine = Default::default();
    let mut names = Default::default();
    let variable = insert_variable_from_line(
        &mut names,
        &mut engine,
        LocatedString::test_new(2, 1, "foo := bar"),
    );
    let expected_sensitivity = mk_sensitivity(&[variable]);

    let val = ast.eval(&mut names, &mut engine);
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

mod substitution {
    use super::*;

    #[test]
    fn simple() {
        crate::test::setup();
        let block = single_block("$(foo:.c=.o)");
        let ast = ast_parse!(block);

        let mut engine = Default::default();
        let mut names = Default::default();
        let variable = insert_variable_from_line(
            &mut names,
            &mut engine,
            LocatedString::test_new(2, 1, "foo := a.c"),
        );
        let expected_sensitivity = mk_sensitivity(&[variable]);

        let val = ast.eval(&mut names, &mut engine);
        info!("Evaluation of AST: {:?}", val.into_string());
        assert_eq!(
            val,
            Block::new(
                expected_sensitivity.clone(),
                vec![evaluated::substitution_reference(
                    block_from_reference(evaluated::constant(LocatedString::test_new(1, 3, "foo"))),
                    block_from_reference(evaluated::constant(LocatedString::test_new(1, 7, ".c"))),
                    block_from_reference(evaluated::constant(LocatedString::test_new(1, 10, ".o"))),
                    Block::new(
                        expected_sensitivity,
                        vec![
                            evaluated::constant(LocatedString::test_new(2, 8, "a")),
                            evaluated::constant(LocatedString::test_new(1, 10, ".o")),
                        ]
                    )
                )]
            )
        )
    }

    #[test]
    fn extra_equals() {
        let block = single_block("$(foo:.c==.o)");
        let ast = ast_parse!(block);

        let mut engine = Default::default();
        let mut names = Default::default();
        let variable = insert_variable_from_line(
            &mut names,
            &mut engine,
            LocatedString::test_new(2, 1, "foo := a.c="),
        );
        let expected_sensitivity = mk_sensitivity(&[variable]);

        let val = ast.eval(&mut names, &mut engine);
        assert_eq!(
            val,
            Block::new(
                expected_sensitivity.clone(),
                vec![evaluated::substitution_reference(
                    block_from_reference(evaluated::constant(LocatedString::test_new(1, 3, "foo"))),
                    block_from_reference(evaluated::constant(LocatedString::test_new(1, 7, ".c="))),
                    block_from_reference(evaluated::constant(LocatedString::test_new(1, 11, ".o"))),
                    Block::new(
                        expected_sensitivity,
                        vec![
                            evaluated::constant(LocatedString::test_new(2, 8, "a")),
                            evaluated::constant(LocatedString::test_new(1, 11, ".o")),
                        ]
                    )
                )]
            )
        )
    }

    #[test]
    fn pre_key() {
        let block = single_block("$(foo:a%c=%)");
        let ast = ast_parse!(block);

        let mut engine = Default::default();
        let mut names = Default::default();
        let variable = insert_variable_from_line(
            &mut names,
            &mut engine,
            LocatedString::test_new(2, 1, "foo := abc"),
        );
        let expected_sensitivity = mk_sensitivity(&[variable]);

        let val = ast.eval(&mut names, &mut engine);
        assert_eq!(
            val,
            Block::new(
                expected_sensitivity.clone(),
                vec![evaluated::substitution_reference(
                    block_from_reference(evaluated::constant(LocatedString::test_new(1, 3, "foo"))),
                    block_from_reference(evaluated::constant(LocatedString::test_new(1, 7, "a%c"))),
                    block_from_reference(evaluated::constant(LocatedString::test_new(1, 11, "%"))),
                    Block::new(
                        expected_sensitivity,
                        vec![evaluated::constant(LocatedString::test_new(2, 9, "b"))]
                    )
                )]
            )
        )
    }

    #[test]
    fn pre_both() {
        let block = single_block("$(foo:a%c=d%f)");
        let ast = ast_parse!(block);

        let mut engine = Default::default();
        let mut names = Default::default();
        let variable = insert_variable_from_line(
            &mut names,
            &mut engine,
            LocatedString::test_new(2, 1, "foo := abc"),
        );
        let expected_sensitivity = mk_sensitivity(&[variable]);

        let val = ast.eval(&mut names, &mut engine);
        assert_eq!(
            val,
            Block::new(
                expected_sensitivity.clone(),
                vec![evaluated::substitution_reference(
                    block_from_reference(evaluated::constant(LocatedString::test_new(1, 3, "foo"))),
                    block_from_reference(evaluated::constant(LocatedString::test_new(1, 7, "a%c"))),
                    block_from_reference(evaluated::constant(LocatedString::test_new(
                        1, 11, "d%f"
                    ))),
                    Block::new(
                        expected_sensitivity,
                        vec![
                            evaluated::constant(LocatedString::test_new(1, 11, "d")),
                            evaluated::constant(LocatedString::test_new(2, 9, "b")),
                            evaluated::constant(LocatedString::test_new(1, 13, "f"))
                        ]
                    )
                )]
            )
        )
    }

    #[test]
    fn multiple() {
        let block = single_block("$(foo:.c=.o)");
        let ast = ast_parse!(block);

        let mut engine = Default::default();
        let mut names = Default::default();
        let variable = insert_variable_from_line(
            &mut names,
            &mut engine,
            LocatedString::test_new(2, 1, "foo := 1.c 2.c"),
        );
        let expected_sensitivity = mk_sensitivity(&[variable]);

        let val = ast.eval(&mut names, &mut engine);
        assert_eq!(
            val,
            Block::new(
                expected_sensitivity.clone(),
                vec![evaluated::substitution_reference(
                    block_from_reference(evaluated::constant(LocatedString::test_new(1, 3, "foo"))),
                    block_from_reference(evaluated::constant(LocatedString::test_new(1, 7, ".c"))),
                    block_from_reference(evaluated::constant(LocatedString::test_new(1, 10, ".o"))),
                    Block::new(
                        expected_sensitivity,
                        vec![
                            evaluated::constant(LocatedString::test_new(2, 8, "1")),
                            evaluated::constant(LocatedString::test_new(1, 10, ".o")),
                            evaluated::constant(LocatedString::synthetic_new(" ")),
                            evaluated::constant(LocatedString::test_new(2, 12, "2")),
                            evaluated::constant(LocatedString::test_new(1, 10, ".o"))
                        ]
                    )
                )]
            )
        )
    }

}

#[test]
fn recursive() {
    let block = single_block("$($(foo))");
    let ast = ast_parse!(block);

    let mut engine = Default::default();
    let mut names = Default::default();
    let foo = insert_variable_from_line(
        &mut names,
        &mut engine,
        LocatedString::test_new(2, 1, "foo := bar"),
    );
    let bar = insert_variable_from_line(
        &mut names,
        &mut engine,
        LocatedString::test_new(3, 1, "bar := baz"),
    );

    let foo_senstivity = mk_sensitivity(&[foo]);
    let overall_sensitivity = mk_sensitivity(&[foo, bar]);

    let val = ast.eval(&mut names, &mut engine);

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

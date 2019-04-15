//! Tests the rule parser
use super::create_span;
use crate::parsers::ParserState;
use crate::Engine;

#[test]
fn simple_rule() {
    crate::test::setup();

    let block = create_span(
        r#"
a: b
	echo bar
"#,
    );

    let mut engine: Engine = Default::default();
    let mut parse_state = ParserState::new();
    let mut names = Default::default();

    // Empty first line
    let (i, _) = assert_ok!(parse_state.parse_line(block.span(), &mut names, &mut engine));

    // rule line
    let (i, _) = assert_ok!(parse_state.parse_line(i, &mut names, &mut engine));

    // recipe line
    let (i, _) = assert_ok!(parse_state.parse_line(i, &mut names, &mut engine));

    // last line
    let (i, _) = assert_ok!(parse_state.parse_line(i, &mut names, &mut engine));
    assert_complete!(i);

    parse_state.close_rule(&mut names, &mut engine);

    let target_name = names.file_name("a").expect("Should have target name");
    let rule = engine
        .database
        .get_rule(target_name)
        .expect("Should have generated rule");
    let dep_name = names.file_name("b").expect("Should have dep name");

    assert!(rule.deps.len() == 1);
    assert!(rule.dep_names.len() == 1);
    assert!(rule.dep_names.contains(&dep_name));
}

#[test]
fn add_targets() {
    crate::test::setup();
    let block = create_span(
        r#"
a: b
	@echo bar

a: c
"#,
    );

    let mut engine: Engine = Default::default();
    let mut parse_state = ParserState::new();
    let mut names = crate::NameCache::default();

    let target_name = names.intern_file_name("a".into());
    let dep1_name = names.intern_file_name("b".into());
    let dep2_name = names.intern_file_name("c".into());

    // Empty first line
    let (i, _) = assert_ok!(parse_state.parse_line(block.span(), &mut names, &mut engine));

    // rule line
    debug!("Rule 1 line");
    let (i, _) = assert_ok!(parse_state.parse_line(i, &mut names, &mut engine));

    // recipe line
    debug!("Recipe line");
    let (i, _) = assert_ok!(parse_state.parse_line(i, &mut names, &mut engine));

    // Empty line
    debug!("Empty line");
    let (i, _) = assert_ok!(parse_state.parse_line(i, &mut names, &mut engine));

    // add dep line
    debug!("Rule 2 line");
    let (i, _) = assert_ok!(parse_state.parse_line(i, &mut names, &mut engine));

    debug!("database state: {:?}", engine.database);

    // The previous rule should have been closed at this point
    let rule = engine
        .database
        .get_rule(target_name)
        .expect("Should have generated rule");

    assert!(rule.deps.len() == 1);
    assert!(rule.dep_names.contains(&dep1_name));

    // last line
    let (i, _) = assert_ok!(parse_state.parse_line(i, &mut names, &mut engine));
    assert_complete!(i);

    parse_state.close_rule(&mut names, &mut engine);

    let rule = engine
        .database
        .get_rule(target_name)
        .expect("Should have modified rule");

    info!("Dependencies are {:?}", rule.deps);

    assert!(rule.deps.len() == 2);
    assert!(rule.dep_names.len() == 2);
    assert!(rule.dep_names.contains(&dep1_name));
    assert!(rule.dep_names.contains(&dep2_name));
}

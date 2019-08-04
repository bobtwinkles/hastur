//! Various utility macros

#[macro_export]
macro_rules! variable_set_to (
    ($names:expr, $engine:ident, $variable_name:expr, $value:expr) => {{
        let names = &mut $names;
        let database = $engine.database.clone();
        let engine = &mut $engine;
        let variable_name = $variable_name;
        let value = $value;

        let variable_name = names
            .variable_name(variable_name)
            .expect(&format!(
                "Variable named {:?} should have been interned",
                variable_name
            ));

        assert_eq!(
            database
                .get_variable(variable_name)
                .expect(&format!("Variable named {:?} should have a value", variable_name))
                .expand(names, engine),
            value
        )
    }}
);

#[macro_export]
macro_rules! has_target_with_deps (
    ($names:expr, $engine:ident, $target_name:expr, $deps:expr) => {{
        let names = &mut $names;
        let database = $engine.database.clone();
        let target_name = $target_name;
        let deps: &[&str] = &$deps;

        let target = names
            .file_name(target_name)
            .expect(&format!("File named {:?} should have been interned", target_name));

        let rule = database.rule_for_target(target);
        let rule = rule.expect(&format!("No rule found for target {:?}", target_name));
        let computed_deps: Vec<String> = rule.dependencies().iter().map(|v| { v.into_string() }).collect();
        let computed_deps_refs: Vec<_> = computed_deps.iter().map(|x| x.as_str()).collect();

        assert_eq!(deps, computed_deps_refs.as_slice());
    }}
);

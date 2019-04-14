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

//! Tests for the define keyword
use super::*;

macro_rules! simple_test {
    ($line:expr, $flavor:expr, $name:expr) => {{
        let mut engine = Default::default();
        let mut names = Default::default();
        let block = create_span($line);
        let (remaining, variable_action) =
            assert_ok!(parse_line(block.span(), &mut names, &mut engine));
        assert_complete!(remaining);

        match variable_action.action {
            Action::Define(variable_data) => {
                assert_eq!(variable_data.flavor, $flavor);
            }
            e => panic!("Unexpected action {:?}", e),
        }

        assert_eq!(
            names.variable_name($name).expect("name was not interned"),
            variable_action.name
        );
    }};
}

#[test]
fn recursive() {
    simple_test!("define foo =", Flavor::Recursive, "foo");
}

#[test]
fn implicit_recursive() {
    simple_test!("define foo bar", Flavor::Recursive, "foo bar");
}

#[test]
fn posix() {
    simple_test!("define foo ::=", Flavor::Simple, "foo");
}

#[test]
fn simple_but_with_space() {
    // N.B. that this variable can never be referenced since Make will parse it
    // as a substitution reference. It's still valid syntax though
    simple_test!("define foo bar :=", Flavor::Recursive, "foo bar :=");
}

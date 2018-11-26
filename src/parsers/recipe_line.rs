//! Parser for recipe lines

use super::{ends_with_backslash, makefile_grab_line};
use crate::{ParseErrorKind, Span};
use nom::IResult;

/// Recognizes a recipe line
pub(super) fn recipe_line<'a>(
    i: Span<'a>,
    command_char: char,
) -> IResult<Span<'a>, String, ParseErrorKind> {
    macro_rules! grab_command_line(
        ($i:expr ) => { grab_command_line!($i , ) };
        ($i:expr, ) => {
            preceded!(
                $i,
                fix_error!(ParseErrorKind, char!(command_char)),
                makefile_grab_line
            )};
    );

    // Grab the first command line
    let (mut i, (line, end_reason)) = add_return_error!(
        i,
        ErrorKind::Custom(ParseErrorKind::RecipeExpected),
        grab_command_line!()
    )?;
    let mut line = line.fragment.0.to_owned();

    // If the line has a comment, stop the line (and processing)
    if end_reason == super::LineEndReason::Comment {
        return Ok((i, line));
    }

    while ends_with_backslash(&line) & 1 == 1 {
        line.push('\n'); // Add back the \n we striped
        let next_line_result = grab_command_line!(i);
        match next_line_result {
            Ok((new_i, (next_line, end_reason))) => {
                line.insert_str(line.len(), next_line.fragment.0);
                i = new_i;

                // If the line has a comment, stop the line (and processing)
                if end_reason == super::LineEndReason::Comment {
                    return Ok((i, line));
                }
            }
            Err(_) => {
                // If we couldn't grab a line, this must not be a command line
                break;
            }
        }
    }

    Ok((i, line))
}

#[cfg(test)]
mod test {
    use super::recipe_line;
    use crate::parsers::test::*;
    use crate::ParseErrorKind;
    use nom::{error_to_list, ErrorKind};

    #[test]
    fn single_line() {
        let test_span = create_span("\ta\n");
        let parse = recipe_line(test_span, '\t');

        assert_eq!(parse, Ok((leftover_span("", 3, 2), String::from("a"))));
    }

    #[test]
    fn at_eof() {
        let test_span = create_span("\ta");
        let parse = recipe_line(test_span, '\t');

        assert_eq!(parse, Ok((leftover_span("", 2, 1), String::from("a"))));
    }

    #[test]
    fn multi_line_cont() {
        let test_span = create_span("\ta\\\n\tb");
        let parse = recipe_line(test_span, '\t');

        assert_eq!(parse, Ok((leftover_span("", 6, 2), String::from("a\\\nb"))));
    }

    #[test]
    fn multi_line_not_cont() {
        let test_span = create_span("\ta\n\tb");
        let parse = recipe_line(test_span, '\t');

        assert_eq!(parse, Ok((leftover_span("\tb", 3, 2), String::from("a"))));
    }

    #[test]
    fn multi_line_not_command() {
        let test_span = create_span("\ta\n\n");
        let parse = recipe_line(test_span, '\t');

        assert_eq!(parse, Ok((leftover_span("\n", 3, 2), String::from("a"))));
    }

    #[test]
    fn multi_line_but_comment() {
        let test_span = create_span("\ta # \n\n");
        let parse = recipe_line(test_span, '\t');

        assert_eq!(
            parse,
            Ok((leftover_span("# \n\n", 3, 1), String::from("a ")))
        );
    }

    #[test]
    fn does_not_match_no_prefix() {
        let test_span = create_span("this is not a recipe line");
        let parse = recipe_line(test_span, '\t');

        assert!(parse.is_err());
        let parse = parse.err().unwrap();
        assert!(match parse {
            nom::Err::Error(_) => true,
            _ => false,
        });
        let context = error_context(parse);
        assert!(context.is_some());
        let context = context.unwrap();
        let errors = error_to_list(&context);
        assert!(error_list_contains(
            &errors,
            ErrorKind::Custom(ParseErrorKind::RecipeExpected)
        ));
        // assert_eq!(parse, (leftover_span("a", 32, 2), ()));
    }
}

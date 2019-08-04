//! Parser for recipe lines

use super::{lift_collapsed_span_error, makefile_grab_line};
use crate::ast::AstNode;
use crate::evaluated::{Block, BlockSpan, ContentReference};
use crate::parsers::ast::parse_ast;
use crate::ParseErrorKind;
use nom::IResult;
use std::sync::Arc;

/// Recognizes a recipe line
pub(super) fn recipe_line<'a>(
    i: BlockSpan<'a>,
    command_char: char,
) -> IResult<BlockSpan<'a>, AstNode, ParseErrorKind> {
    use nom::{InputIter, Slice};
    debug!("Get recipe from {:?}", i.into_string());

    let (i, (input_line, _)) = add_return_error!(
        i,
        ErrorKind::Custom(ParseErrorKind::RecipeExpected),
        preceded!(
            fix_error!(ParseErrorKind, char!(command_char)),
            makefile_grab_line
        )
    )?;

    let line_start = i;

    let mut fragments: Vec<ContentReference> = Vec::new();

    let mut it = input_line.iter_indices();
    let mut push_start = 0;
    let mut strip_next_if_cmd = false;
    for (idx, ch) in it {
        // XXX: This doesn't handle Windows newlines
        if ch == '\n' {
            fragments.push(input_line.slice(push_start..idx + 1).to_content_reference());
            strip_next_if_cmd = true;
            push_start = idx + 1;
        }
        if ch == '\t' && strip_next_if_cmd {
            push_start = idx + 1;
            strip_next_if_cmd = false;
        }
    }

    if push_start < input_line.len() {
        fragments.push(input_line.slice(push_start..).to_content_reference());
    }

    let line = Block::new(i.parent().raw_sensitivity(), fragments);

    let (_, ast) = parse_ast(line.span()).map_err(|e| lift_collapsed_span_error(e, i))?;

    Ok((i, ast))
}

#[cfg(test)]
mod test {
    use super::recipe_line;
    use crate::ast;
    use crate::parsers::test;
    use crate::parsers::test::create_span;
    use crate::source_location::{LocatedString, Location};
    use crate::ParseErrorKind;
    use nom::{error_to_list, ErrorKind};
    use pretty_assertions::assert_eq;

    #[test]
    fn single_line() {
        let test_span = create_span("\ta\n");
        let test_span = test_span.span();
        let parse = assert_ok!(recipe_line(test_span, '\t'));
        assert_complete!(parse.0);
        assert_eq!(parse.1, ast::constant(LocatedString::test_new(1, 2, "a")));
    }

    #[test]
    fn at_eof() {
        let test_span = create_span("\ta");
        let test_span = test_span.span();
        let parse = assert_ok!(recipe_line(test_span, '\t'));
        assert_complete!(parse.0);
        assert_eq!(parse.1, ast::constant(LocatedString::test_new(1, 2, "a")));
    }

    #[test]
    fn multi_line_cont() {
        crate::test::setup();

        let test_span = create_span("\ta\\\n\tb");
        let test_span = test_span.span();
        let parse = assert_ok!(recipe_line(test_span, '\t'));
        assert_complete!(parse.0);
        assert_eq!(
            parse.1,
            ast::collapsing_concat(
                Location::test_location(1, 2),
                vec![
                    ast::constant(LocatedString::test_new(1, 2, "a\\\n")),
                    ast::constant(LocatedString::test_new(2, 2, "b"))
                ]
            )
        );
    }

    #[test]
    fn multi_line_not_cont() {
        let test_span = create_span("\ta\n\tb");
        let test_span = test_span.span();
        let parse = assert_ok!(recipe_line(test_span, '\t'));
        assert_segments_eq!(parse.0, [("\tb", Location::test_location(2, 1))]);
        assert_eq!(parse.1, ast::constant(LocatedString::test_new(1, 2, "a")));
    }

    #[test]
    fn multi_line_not_command() {
        let test_span = create_span("\ta\n\n");
        let test_span = test_span.span();
        let parse = assert_ok!(recipe_line(test_span, '\t'));
        assert_segments_eq!(parse.0, [("\n", Location::test_location(2, 1))]);
        assert_eq!(parse.1, ast::constant(LocatedString::test_new(1, 2, "a")));
    }

    #[test]
    fn multi_line_but_comment() {
        let test_span = create_span("\ta # \n\n");
        let test_span = test_span.span();
        let parse = assert_ok!(recipe_line(test_span, '\t'));
        assert_segments_eq!(parse.0, [("# \n\n", Location::test_location(1, 4))]);
        assert_eq!(parse.1, ast::constant(LocatedString::test_new(1, 2, "a ")));
    }

    #[test]
    fn does_not_match_no_prefix() {
        let test_span = create_span("this is not a recipe line");
        let test_span = test_span.span();
        let parse = recipe_line(test_span, '\t');

        assert!(parse.is_err());
        let parse = parse.err().unwrap();
        assert!(match parse {
            nom::Err::Error(_) => true,
            _ => false,
        });
        let context = test::error_context(parse);
        assert!(context.is_some());
        let context = context.unwrap();
        let errors = error_to_list(&context);
        assert!(test::error_list_contains(
            &errors,
            ErrorKind::Custom(ParseErrorKind::RecipeExpected)
        ));
    }
}

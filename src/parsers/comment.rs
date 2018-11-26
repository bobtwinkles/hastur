//! Parses comments
use super::{ends_with_backslash, makefile_grab_line, makefile_whitespace};
use crate::{ParseErrorKind, Span};
use nom::IResult;

/// Snag all the comment content
/// Expects to start on a '#'
pub(super) fn parse_comment<'a>(i: Span<'a>) -> IResult<Span<'a>, (), ParseErrorKind> {
    let (mut i, mut line_parse) = preceded!(
        i,
        add_return_error!(
            ErrorKind::Custom(ParseErrorKind::CommentExpected),
            fix_error!(ParseErrorKind, char!('#'))
        ),
        makefile_grab_line
    )?;

    // While the line grab was terminated by another comment start or the line ends with a backslash,
    // keep grabbing lines
    while line_parse.1 == super::LineEndReason::Comment
        || ends_with_backslash(line_parse.0.fragment.0) & 1 == 1
    {
        let output = preceded!(
            i,
            fix_error!(ParseErrorKind, opt!(char!('#'))),
            makefile_grab_line
        )?;
        i = output.0;
        line_parse = output.1;
    }

    Ok((i, ()))
}

/// Parse a comment
pub(super) fn parse_comment_following_whitespace<'a>(
    i: Span<'a>,
) -> IResult<Span<'a>, (), ParseErrorKind> {
    // Snag all the whitespace, and then delegate to the non-whitespace-aware comment parser
    preceded!(i, makefile_whitespace, parse_comment)
}

#[cfg(test)]
mod test {
    use super::{parse_comment, parse_comment_following_whitespace};
    use crate::parsers::test::*;
    use crate::ParseErrorKind;
    use nom::{error_to_list, ErrorKind};

    #[test]
    fn does_not_match_not_comment() {
        let test_span = create_span("this is not a comment");
        let parse = parse_comment(test_span);

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
            ErrorKind::Custom(ParseErrorKind::CommentExpected)
        ));
        // assert_eq!(parse, (leftover_span("a", 32, 2), ()));
    }

    #[test]
    fn single_line() {
        let test_span = create_span("# this is a single line comment\na");
        let parse = parse_comment(test_span);

        assert_eq!(parse, Ok((leftover_span("a", 32, 2), ())));
    }

    #[test]
    fn comment_with_hashes() {
        let test_span = create_span("# this comment has hashes # oh no\na");
        let parse = parse_comment(test_span);

        assert_eq!(parse, Ok((leftover_span("a", 34, 2), ())));
    }

    #[test]
    fn multi_line_comment() {
        let test_span = create_span("# this comment \\\nhas line breaks\\\noh no\na");
        let parse = parse_comment(test_span);

        assert_eq!(parse, Ok((leftover_span("a", 40, 4), ())));
    }

    #[test]
    fn preceding_spaces() {
        let test_span = create_span(" \t# this comment \\nhas line breaks\\\noh no\na");
        let parse = parse_comment_following_whitespace(test_span);

        assert_eq!(parse, Ok((leftover_span("a", 42, 3), ())));
    }
}

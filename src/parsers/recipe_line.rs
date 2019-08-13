//! Parser for recipe lines

use super::{lift_collapsed_span_error, makefile_grab_line};
use crate::ast::AstNode;
use crate::evaluated::{Block, BlockSpan, ContentReference};
use crate::parsers::ast::parse_ast;
use crate::tokenizer::{iterator_to_token_stream, TokenType, VariableKind};
use crate::ParseErrorKind;
use nom::IResult;

/// Recognizes a recipe line
pub(super) fn recipe_line<'a>(
    i: BlockSpan<'a>,
    command_char: char,
) -> IResult<BlockSpan<'a>, AstNode, ParseErrorKind> {
    use nom::{InputIter, Slice};
    debug!("Get recipe from {:?}", i.into_string());

    let line_start = i;

    let (i, (input_line, _)) = add_return_error!(
        i,
        ErrorKind::Custom(ParseErrorKind::RecipeExpected),
        preceded!(
            fix_error!(ParseErrorKind, char!(command_char)),
            makefile_grab_line
        )
    )?;

    let mut fragments: Vec<ContentReference> = Vec::new();

    let it = input_line.iter_indices();
    let mut tokens = iterator_to_token_stream(it);

    #[derive(Copy, Clone, Debug, PartialEq, Eq)]
    enum VarState {
        Outside,
        InsideParen(usize),
        InsideBrace(usize),
    }

    let mut push_start = 0;
    let mut last_non_whitespace_end = 0;
    let mut inside_variable_reference = VarState::Outside;
    let mut ignoring_whitespace = false;

    while let Some(tok) = tokens.next() {
        macro_rules! saw_non_whitespace {
            () => {
                last_non_whitespace_end = tok.end;
                ignoring_whitespace = false;
            };
        };

        debug!("Process token {:?}", tok);

        match tok.token_type {
            TokenType::EscapedCharacter(Some('\n')) => {
                macro_rules! ignore_command_char {
                    () => {
                        if tokens.peek_next_character() == Some(command_char) {
                            // unwrap is safe because we just peeked it
                            let (start, _) = tokens.consume_next_character().unwrap();
                            push_start = start + command_char.len_utf8();
                        }
                    };
                }
                if inside_variable_reference != VarState::Outside {
                    // Inside variable references, we replace all the whitespace
                    // around the newline with a single space

                    // If we're ignoring whitespace, that means we've already
                    // pushed a synthetic space and should mostly just ignore
                    // this newline. We also want to drop the command character
                    // if we see it.
                    if ignoring_whitespace {
                        ignore_command_char!();
                        continue;
                    }

                    // We know we need everything up to the last non whitespace character
                    fragments.push(
                        input_line
                            .slice(push_start..last_non_whitespace_end)
                            .to_content_reference(),
                    );
                    // We need a sythetic space
                    fragments.push(ContentReference::space());

                    push_start = tok.end;

                    // Finally, if the character immediately following this line
                    // break is the command char, drop it. We also want to
                    // ignore any subsequent whitespace until something
                    // interesting shows up
                    ignore_command_char!();
                    ignoring_whitespace = true;
                } else {
                    // Outside of variable references, we scan out everything
                    // until the end of this token, and then maybe ignore the
                    // command character
                    fragments.push(input_line.slice(push_start..tok.end).to_content_reference());
                    push_start = tok.end;

                    ignore_command_char!();
                }
            }
            TokenType::Whitespace => {
                if ignoring_whitespace {
                    push_start = tok.end;
                }
            }
            TokenType::VariableReference(VariableKind::OpenParen) => {
                inside_variable_reference = match inside_variable_reference {
                    VarState::Outside => VarState::InsideParen(1),
                    VarState::InsideParen(x) => VarState::InsideParen(x + 1),
                    VarState::InsideBrace(x) => VarState::InsideBrace(x),
                };

                saw_non_whitespace!();
            }
            TokenType::VariableReference(VariableKind::OpenBrace) => {
                inside_variable_reference = match inside_variable_reference {
                    VarState::Outside => VarState::InsideBrace(1),
                    VarState::InsideBrace(x) => VarState::InsideBrace(x + 1),
                    VarState::InsideParen(x) => VarState::InsideParen(x),
                };

                saw_non_whitespace!();
            }
            TokenType::CloseParen => {
                inside_variable_reference = match inside_variable_reference {
                    VarState::Outside => VarState::Outside,
                    VarState::InsideParen(1) => VarState::Outside,
                    VarState::InsideParen(x) => VarState::InsideParen(x - 1),
                    VarState::InsideBrace(x) => VarState::InsideBrace(x),
                };

                saw_non_whitespace!();
            }
            TokenType::CloseBrace => {
                inside_variable_reference = match inside_variable_reference {
                    VarState::Outside => VarState::Outside,
                    VarState::InsideBrace(1) => VarState::Outside,
                    VarState::InsideBrace(x) => VarState::InsideBrace(x - 1),
                    VarState::InsideParen(x) => VarState::InsideParen(x),
                };

                saw_non_whitespace!();
            }
            _ => {
                // XXX: We don't handle tok.skipped here because the only time
                // that gets set is when a $$ is encountered. That handling
                // really should be contained to parsers/ast anyway, and not
                // leaking all over the code.
                saw_non_whitespace!();
            }
        }
    }

    if push_start < input_line.len() {
        fragments.push(input_line.slice(push_start..).to_content_reference());
    }

    let line = Block::new(i.parent().raw_sensitivity(), fragments);

    let (_, ast) = parse_ast(line.span()).map_err(|e| lift_collapsed_span_error(e, line_start))?;

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

    macro_rules! recipe_line_test {
        ($span_contents:expr, $out_name:ident) => {
            crate::test::setup();

            let test_span = create_span($span_contents);
            let test_span = test_span.span();

            let $out_name = assert_ok!(recipe_line(test_span, '\t'));
        };
    }

    #[test]
    fn single_line() {
        recipe_line_test!("\ta\n", parse);
        assert_complete!(parse.0);
        assert_eq!(parse.1, ast::constant(LocatedString::test_new(1, 2, "a")));
    }

    #[test]
    fn at_eof() {
        recipe_line_test!("\ta", parse);
        assert_complete!(parse.0);
        assert_eq!(parse.1, ast::constant(LocatedString::test_new(1, 2, "a")));
    }

    #[test]
    fn multi_line_cont() {
        recipe_line_test!("\ta\\\n\tb", parse);
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
        recipe_line_test!("\ta\n\tb", parse);
        assert_segments_eq!(parse.0, [("\tb", Location::test_location(2, 1))]);
        assert_eq!(parse.1, ast::constant(LocatedString::test_new(1, 2, "a")));
    }

    #[test]
    fn multi_line_not_command() {
        recipe_line_test!("\ta\n\n", parse);
        assert_segments_eq!(parse.0, [("\n", Location::test_location(2, 1))]);
        assert_eq!(parse.1, ast::constant(LocatedString::test_new(1, 2, "a")));
    }

    #[test]
    fn multi_line_but_comment() {
        recipe_line_test!("\ta # \n\n", parse);
        assert_segments_eq!(parse.0, [("# \n\n", Location::test_location(1, 4))]);
        assert_eq!(parse.1, ast::constant(LocatedString::test_new(1, 2, "a ")));
    }

    #[test]
    fn collapse_parens_escapes() {
        recipe_line_test!("\techo '(a \\\\\\\n\t b)", parse);
        assert_complete!(parse.0);
        assert_eq!(
            parse.1,
            ast::collapsing_concat(
                Location::test_location(1, 2),
                vec![
                    ast::constant(LocatedString::test_new(1, 2, "echo '(a \\\\\\\n")),
                    ast::constant(LocatedString::test_new(2, 2, " b)"))
                ]
            )
        )
    }

    #[test]
    fn collapse_parens_no_function() {
        recipe_line_test!("\techo '(a \\\n\t b)'", parse);
        assert_complete!(parse.0);
        assert_eq!(
            parse.1,
            ast::collapsing_concat(
                Location::test_location(1, 2),
                vec![
                    ast::constant(LocatedString::test_new(1, 2, "echo '(a \\\n")),
                    ast::constant(LocatedString::test_new(2, 2, " b)'"))
                ]
            )
        );
    }

    #[test]
    fn collapse_in_function() {
        recipe_line_test!("\techo '$(if t,a \\\n\t b)'", parse);
        assert_complete!(parse.0);
        assert_eq!(
            parse.1,
            ast::collapsing_concat(
                Location::test_location(1, 2),
                vec![
                    ast::constant(LocatedString::test_new(1, 2, "echo '")),
                    ast::if_fn(
                        Location::test_location(1, 8),
                        ast::constant(LocatedString::test_new(1, 13, "t")),
                        ast::collapsing_concat(
                            Location::test_location(1, 15),
                            vec![
                                ast::constant(LocatedString::test_new(1, 15, "a")),
                                ast::constant(LocatedString::new(
                                    Location::Synthetic.into(),
                                    " ".into()
                                )),
                                ast::constant(LocatedString::test_new(2, 3, "b")),
                            ]
                        ),
                        ast::empty(),
                    ),
                    ast::constant(LocatedString::test_new(2, 5, "'"))
                ]
            )
        );
    }

    #[test]
    fn collapse_only_line_in_function() {
        recipe_line_test!("\techo '$(if t,a \\\\\\\n\t b)'", parse);
        assert_complete!(parse.0);
        assert_eq!(
            parse.1,
            ast::collapsing_concat(
                Location::test_location(1, 2),
                vec![
                    ast::constant(LocatedString::test_new(1, 2, "echo '")),
                    ast::if_fn(
                        Location::test_location(1, 8),
                        ast::constant(LocatedString::test_new(1, 13, "t")),
                        ast::collapsing_concat(
                            Location::test_location(1, 15),
                            vec![
                                ast::constant(LocatedString::test_new(1, 15, "a \\\\")),
                                ast::constant(LocatedString::new(
                                    Location::Synthetic.into(),
                                    " ".into()
                                )),
                                ast::constant(LocatedString::test_new(2, 3, "b")),
                            ]
                        ),
                        ast::empty()
                    ),
                    ast::constant(LocatedString::test_new(2, 5, "'"))
                ]
            )
        )
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

//! Parses a variable expansion AST out of the provided block span
use crate::ast;
use crate::ast::AstNode;
use crate::evaluated::BlockSpan;
use crate::parsers::{fail_out, makefile_whitespace};
use crate::source_location::Location;
use crate::tokenizer::{
    iterator_to_token_stream, BuiltinFunction, TokenStream, TokenType, VariableKind,
};
use crate::ParseErrorKind;
use nom::{Err, ErrorKind, IResult, InputIter, Slice};

#[cfg(test)]
mod test;

/// Extract a semantic variable evaluation AST from the provided block span
pub(crate) fn parse_ast<'a>(i: BlockSpan<'a>) -> IResult<BlockSpan<'a>, AstNode, ParseErrorKind> {
    let start_location = match i.location() {
        Some(v) => v,
        None => return Ok((i, ast::empty())),
    };
    let mut master_concat_nodes = Vec::new();
    let mut tokens = iterator_to_token_stream(i.iter_indices());
    let mut start_index = 0;
    let mut prev_end = 0;

    // This loop is a simplified version of the one in
    // accumulate_reference_content. Any fixes applied here probably need to go
    // there as well.
    while let Some(tok) = tokens.next() {
        debug!("Top level in ast: {:?}", tok);
        match tok.token_type {
            TokenType::VariableReference(kind) => {
                // We have encountered a variable reference, push all prior content
                let prior = i.slice(start_index..tok.start);
                for segment in prior.segments() {
                    master_concat_nodes.push(ast::constant(segment.into()));
                }

                // And process the reference
                let (end, content) =
                    variable_reference_start(i, &mut tokens, tok.start, tok.end, kind)?;
                master_concat_nodes.push(content);

                start_index = end;
                prev_end = end;
            }
            _ => {
                // For any other token, make sure that it is actually a direct
                // continuation of the previous block. If it isn't, dump and continue
                if tok.start != prev_end {
                    // Something indicated we should skip characters, dump everything and continue.
                    debug!("Something caused a token skip from {:?} to {:?}. Pushing content between {:?} and {:?}", prev_end, tok.start, start_index, prev_end);
                    let prior = i.slice(start_index..prev_end);
                    for segment in prior.segments() {
                        master_concat_nodes.push(ast::constant(segment.into()));
                    }

                    start_index = tok.start;
                }
                prev_end = tok.end;
            }
        }
    }

    debug!("Last index pushed was {:?}", start_index);
    if start_index != i.len() {
        let tail = i.slice(start_index..);
        for segment in tail.segments() {
            master_concat_nodes.push(ast::constant(segment.into()));
        }
    }

    Ok((
        i.slice(i.len()..),
        ast::collapsing_concat(start_location, master_concat_nodes),
    ))
}

fn variable_reference_start<'a, IT: Iterator<Item = (usize, char)>>(
    i: BlockSpan<'a>,
    tok_iterator: &mut TokenStream<IT>,
    token_start: usize,
    token_end: usize,
    var_kind: VariableKind,
) -> Result<(usize, AstNode), nom::Err<BlockSpan<'a>, ParseErrorKind>> {
    match var_kind {
        VariableKind::SingleCharacter => {
            let reference = i.slice(token_start..token_end);
            let dollar = reference.slice(..1);
            let name = reference.slice(1..);

            Ok((
                token_end,
                ast::variable_reference(
                    dollar.location().unwrap(),
                    ast::collapsing_concat(
                        name.location().unwrap(),
                        name.segments()
                            .map(|segment| ast::constant(segment.into()))
                            .collect(),
                    ),
                ),
            ))
        }
        VariableKind::OpenParen => {
            // recursive variable reference for parens
            // Note that this will automatically eat its matching
            // CloseParen, so we know if we see a CloseParen (and our
            // close_token is CloseParen) it's ours.
            let dollar_location = i.slice(token_start..token_end).location().unwrap();
            parse_var_ref(
                i,
                tok_iterator,
                token_end,
                dollar_location,
                TokenType::CloseParen,
            )
        }
        VariableKind::OpenBrace => {
            // recursive variable reference for braces
            // See Note above
            let dollar_location = i.slice(token_start..token_end).location().unwrap();
            parse_var_ref(
                i,
                tok_iterator,
                token_end,
                dollar_location,
                TokenType::CloseBrace,
            )
        }
        VariableKind::Unterminated => {
            let content = i.slice(token_start..token_end);

            Ok((
                token_end,
                ast::collapsing_concat(
                    content.location().unwrap(),
                    content
                        .segments()
                        .map(|segment| ast::constant(segment.into()))
                        .collect(),
                ),
            ))
        }
    }
}

fn parse_var_ref<'a, IT: Iterator<Item = (usize, char)>>(
    i: BlockSpan<'a>,
    tok_iterator: &mut TokenStream<IT>,
    start_index: usize,
    dollar_location: Location,
    close_token: TokenType,
) -> Result<(usize, AstNode), nom::Err<BlockSpan<'a>, ParseErrorKind>> {
    assert!(close_token == TokenType::CloseParen || close_token == TokenType::CloseBrace);
    debug!("Parsing variable reference starting at {:?}", start_index);

    let tok = tok_iterator.next().ok_or_else(|| {
        fail_out::<()>(i.slice(start_index..), ParseErrorKind::UnternimatedVariable)
            .err()
            .unwrap()
    })?;

    let start_location = i.slice(tok.start..).location().unwrap();

    match tok.token_type {
        TokenType::BuiltinFunction(func) => potential_function(
            i,
            tok_iterator,
            start_location,
            start_index,
            dollar_location,
            close_token,
            func,
        ),
        TokenType::VariableReference(kind) => {
            // This variable reference immediately dispatches into another
            // variable reference. Process that, and then pick up the rest of
            // the content. This is a kinda weird situation where even if the
            // variable evaluates to a function name, we won't parse it as a
            // function. Fortunately, GNU Make doesn't seem to evaluate
            // functions when their names show up this way.
            //
            // XXX: Validate and then write tests for the above behavior,
            // probably in the eval module.
            let mut master_concat_nodes = Vec::new();

            let (end, content) =
                variable_reference_start(i, tok_iterator, tok.start, tok.end, kind)?;
            debug!(
                "Variable reference at {:?} immediately dispatched into a reference {:?}",
                start_index, content
            );
            let content_location = content.location();
            master_concat_nodes.push(content);

            let end = accumulate_reference_content(
                i,
                tok_iterator,
                end,
                close_token,
                &mut master_concat_nodes,
            )?;

            Ok((
                end,
                ast::variable_reference(
                    dollar_location,
                    ast::collapsing_concat(content_location, master_concat_nodes),
                ),
            ))
        }
        c if c == close_token => {
            debug!(
                "Variable reference at {:?} terminated immediately",
                start_index
            );
            let end = i.slice(start_index..tok.end);
            let content = end.slice(0..0);
            Ok((
                tok.end,
                ast::variable_reference(
                    dollar_location,
                    ast::collapsing_concat(
                        content.location().unwrap(),
                        content
                            .segments()
                            .map(|segment| ast::constant(segment.into()))
                            .collect(),
                    ),
                ),
            ))
        }
        _ => non_function_internal(
            i,
            tok_iterator,
            start_location,
            start_index,
            dollar_location,
            close_token,
        ),
    }
}

fn potential_function<'a, IT: Iterator<Item = (usize, char)>>(
    i: BlockSpan<'a>,
    tok_iterator: &mut TokenStream<IT>,
    start_location: Location,
    start_index: usize,
    dollar_location: Location,
    close_token: TokenType,
    func: BuiltinFunction,
) -> Result<(usize, AstNode), nom::Err<BlockSpan<'a>, ParseErrorKind>> {
    debug!(
        "Variable reference at {:?} might be a function",
        start_index
    );
    let tok = tok_iterator.next().ok_or_else(|| {
        fail_out::<()>(i.slice(start_index..), ParseErrorKind::UnternimatedVariable)
            .err()
            .unwrap()
    })?;

    match tok.token_type {
        TokenType::VariableReference(kind) => {
            // This looked like a function, but it turned out to be a
            // concatenation of a function name and some other variable-esque
            // content. Slurp all that up
            let mut master_concat_nodes = Vec::new();

            // Prefix. This is safe since we know these segments contain
            // precisely the text making up a function keyword.
            for segment in i.slice(start_index..tok.start).segments() {
                master_concat_nodes.push(ast::constant(segment.into()));
            }

            // The variable reference we just hit
            let (end, content) =
                variable_reference_start(i, tok_iterator, tok.start, tok.end, kind)?;
            debug!(
                "Variable reference at {:?} immediately dispatched into a reference {:?}",
                start_index, content
            );
            let content_location = content.location();
            master_concat_nodes.push(content);

            // And any trailing content prior to token end
            let end = accumulate_reference_content(
                i,
                tok_iterator,
                end,
                close_token,
                &mut master_concat_nodes,
            )?;

            Ok((
                tok.end,
                ast::variable_reference(
                    dollar_location,
                    ast::collapsing_concat(content_location, master_concat_nodes),
                ),
            ))
        }
        TokenType::Whitespace => {
            // this is almost certainly a function
            // TODO: determine what Make does in the case of variable names like "if "
            // As far as I know it's impossible to assign to such variable names,
            // but that should be checked
            match func {
                f => unimplemented!("Parsing for function {:?}", f),
            }
        }
        c if c == close_token => {
            // The variable reference terminated immediately, this was a variable with a function name.
            debug!(
                "Variable reference at {:?} is a variable named like a function",
                start_index
            );
            let content = i.slice(start_index..tok.start);
            Ok((
                tok.end,
                ast::variable_reference(
                    dollar_location,
                    ast::collapsing_concat(
                        content.location().unwrap(),
                        content
                            .segments()
                            .map(|segment| ast::constant(segment.into()))
                            .collect(),
                    ),
                ),
            ))
        }
        _ => {
            // This was a regular variable reference after all.
            debug!("Variable reference at {:?} was not a function", start_index);
            non_function_internal(
                i,
                tok_iterator,
                start_location,
                start_index,
                dollar_location,
                close_token,
            )
        }
    }
}

fn non_function_internal<'a, IT: Iterator<Item = (usize, char)>>(
    i: BlockSpan<'a>,
    tok_iterator: &mut TokenStream<IT>,
    start_location: Location,
    start_index: usize,
    dollar_location: Location,
    close_token: TokenType,
) -> Result<(usize, AstNode), nom::Err<BlockSpan<'a>, ParseErrorKind>> {
    debug!(
        "Handling non-function variable internal reference at {:?}",
        start_index
    );
    let mut prev_end = start_index;
    let mut master_concat_nodes = Vec::new();

    let end = accumulate_reference_content(
        i,
        tok_iterator,
        start_index,
        close_token,
        &mut master_concat_nodes,
    )?;

    Ok((
        end,
        ast::variable_reference(
            dollar_location,
            ast::collapsing_concat(start_location, master_concat_nodes),
        ),
    ))
}

fn accumulate_reference_content<'a, IT: Iterator<Item = (usize, char)>>(
    i: BlockSpan<'a>,
    tok_iterator: &mut TokenStream<IT>,
    mut start_index: usize,
    close_token: TokenType,
    master_concat_nodes: &mut Vec<AstNode>,
) -> Result<usize, nom::Err<BlockSpan<'a>, ParseErrorKind>> {
    debug!(
        "Accumulating reference content, starting at {:?}",
        start_index
    );
    let mut prev_end = start_index;

    // Just scan until we hit the close token
    while let Some(tok) = tok_iterator.next() {
        match tok.token_type {
            TokenType::VariableReference(kind) => {
                // We have encountered a variable reference, push all prior content
                let prior = i.slice(start_index..tok.start);
                for segment in prior.segments() {
                    master_concat_nodes.push(ast::constant(segment.into()));
                }

                // And process the reference
                let (end, content) =
                    variable_reference_start(i, tok_iterator, tok.start, tok.end, kind)?;
                master_concat_nodes.push(content);

                start_index = end;
                prev_end = end;
            }
            c if c == close_token => {
                // We've found our close token. Dump everything up to this point and exit.
                if start_index != i.len() {
                    let tail = i.slice(start_index..tok.start);
                    for segment in tail.segments() {
                        master_concat_nodes.push(ast::constant(segment.into()));
                    }
                }

                return Ok(tok.end);
            }
            _ => {
                // any other token accumulates
                debug!(
                    "Accumulating token {:?} {:?} {:?}",
                    tok.start, tok.token_type, tok.end
                );

                if tok.start != prev_end {
                    // Something indicated we should skip characters, dump everything and continue.
                    debug!("Something caused a token skip from {:?} to {:?} in accumulate", prev_end, tok.start);
                    let prior = i.slice(start_index..prev_end);
                    for segment in prior.segments() {
                        master_concat_nodes.push(ast::constant(segment.into()));
                    }

                    start_index = tok.start;
                }

                prev_end = tok.end;
            }
        }
    }

    // We ran out of tokens without matching the close token, that's an error
    fail_out::<()>(i.slice(i.len()..), ParseErrorKind::UnternimatedVariable).map(|_| unreachable!())
}

fn advanced_var<'a>(
    i: BlockSpan<'a>,
    dollar_location: Location,
    start_char: char,
    term_char: char,
) -> IResult<BlockSpan<'a>, AstNode, ParseErrorKind> {
    use nom::{InputIter, InputTake, Slice};
    let mut count = 1;
    let mut it = i.iter_indices();
    let mut split_idx = 0;

    // Iterate to find the balanced character
    while count > 0 {
        let (idx, c) = match it.next() {
            Some(v) => v,
            None => {
                return fail_out(i, ParseErrorKind::UnternimatedVariable);
            }
        };
        split_idx = idx;
        if c == start_char {
            count += 1;
        } else if c == term_char {
            count -= 1;
        };
    }

    let (i, name_node) = i.take_split(split_idx + 1);
    let name_node = name_node.slice(..name_node.len() - 1);

    match function_call(name_node, dollar_location.clone()) {
        Ok(v) => Ok(v),
        Err(Err::Failure(context)) => {
            if context.clone().into_error_kind()
                == nom::ErrorKind::Custom(ParseErrorKind::InternalFailure("not a function call"))
            {
                let parsed = parse_ast(name_node)?;

                // quick sanity check as long as we're testing
                #[cfg(test)]
                assert_complete!(parsed.0);

                Ok((i, ast::variable_reference(dollar_location, parsed.1)))
            } else {
                Err(Err::Failure(context))
            }
        }
        v => v,
    }
}

fn function_call<'a>(
    i: BlockSpan<'a>,
    dollar_location: Location,
) -> IResult<BlockSpan<'a>, AstNode, ParseErrorKind> {
    debug!("Attempting to match function call {:?}", i.into_string());
    fn no_such_function<'a>(i: BlockSpan<'a>) -> IResult<BlockSpan<'a>, AstNode, ParseErrorKind> {
        Err(Err::Failure(nom::Context::Code(
            i,
            ErrorKind::Custom(ParseErrorKind::InternalFailure("not a function call")),
        )))
    }

    macro_rules! func_entry {
        ($i: expr, $t:literal, $f:expr) => {
            pe_complete!(
                $i,
                do_parse!(
                    pe_fix!(tag!($t))
                        >> many1!(makefile_whitespace)
                        >> parsed: apply!($f, dollar_location.clone())
                        >> (parsed)
                )
            )
        };
    }

    alt!(
        i,
        func_entry!("eval", eval)
            | func_entry!("strip", strip)
            | func_entry!("words", words)
            | func_entry!("word", word)
            | pe_complete!(no_such_function)
    )
}

fn function_argument<'a>(
    i: BlockSpan<'a>,
) -> IResult<BlockSpan<'a>, BlockSpan<'a>, ParseErrorKind> {
    use nom::{InputIter, InputTake};

    // Strip off any leading whitespace
    let (i, _) = many0!(i, makefile_whitespace)?;

    let mut it = i.iter_indices();
    let mut paren_count = 0;
    let mut curly_count = 0;
    while let Some((j, c)) = it.next() {
        match c {
            '(' => paren_count += 1,
            ')' => {
                if paren_count > 0 {
                    paren_count -= 1
                } else {
                    // Somehow we ended up with unbalanced parens, abort
                    return fail_out(i, ParseErrorKind::UnternimatedVariable);
                }
            }
            '{' => curly_count += 1,
            '}' => {
                if curly_count > 0 {
                    curly_count -= 1;
                } else {
                    // Somehow we ended up with unbalanced braces, abort
                    return fail_out(i, ParseErrorKind::UnternimatedVariable);
                }
            }
            ',' => {
                if curly_count == 0 && paren_count == 0 {
                    // We're outside any variable references, split just before the comma
                    return Ok(i.take_split(j));
                }
            }
            _ => {}
        }
    }

    Ok(i.take_split(i.len()))
}

fn eval<'a>(
    i: BlockSpan<'a>,
    start_location: Location,
) -> IResult<BlockSpan<'a>, AstNode, ParseErrorKind> {
    let (i, args) = parse_ast(i)?;

    Ok((i, ast::eval(start_location, args)))
}

fn strip<'a>(
    i: BlockSpan<'a>,
    start_location: Location,
) -> IResult<BlockSpan<'a>, AstNode, ParseErrorKind> {
    let (i, arg) = function_argument(i)?;
    if i.len() != 0 {
        return fail_out(i, ParseErrorKind::ExtraArguments("strip"));
    }

    let (_, arg) = parse_ast(arg)?;

    Ok((i, ast::strip(start_location, arg)))
}

fn words<'a>(
    i: BlockSpan<'a>,
    start_location: Location,
) -> IResult<BlockSpan<'a>, AstNode, ParseErrorKind> {
    let (i, arg) = function_argument(i)?;
    if i.len() != 0 {
        return fail_out(i, ParseErrorKind::ExtraArguments("words"));
    }

    let (_, arg) = parse_ast(arg)?;

    Ok((i, ast::words(start_location, arg)))
}

fn word<'a>(
    i: BlockSpan<'a>,
    start_location: Location,
) -> IResult<BlockSpan<'a>, AstNode, ParseErrorKind> {
    let (i, index) = function_argument(i)?;
    let (i, _) = match char!(i, ',') {
        Ok(v) => v,
        Err(_) => return fail_out(i, ParseErrorKind::InsufficientArguments("word")),
    };
    let (_, index) = parse_ast(index)?;

    let (i, list) = function_argument(i)?;
    if i.len() != 0 {
        return fail_out(i, ParseErrorKind::ExtraArguments("word"));
    }
    let (_, list) = parse_ast(list)?;

    Ok((i, ast::word(start_location, index, list)))
}

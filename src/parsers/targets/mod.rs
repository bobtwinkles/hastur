//! Parser for target lines
use super::variable::{self, VariableAction};
use crate::ast::AstNode;
use crate::eval::{Flavor, Origin, VariableParameters};
use crate::evaluated::{Block, BlockSpan, ContentReference};
use crate::parsers::ast::parse_ast;
use crate::parsers::variable::parse_line as parse_variable_line;
use crate::parsers::{
    fail_out, lift_collapsed_span_error, makefile_line, makefile_take_until_unquote,
    makefile_whitespace, ProtoRule,
};
use crate::FileName;
use crate::{Engine, NameCache, ParseErrorKind};
use nom::IResult;
use std::sync::Arc;

#[cfg(test)]
mod test;

/// What action to take in response to this line
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Action {
    /// Turns out there was nothing here
    NoAction,
    /// The start of a new rule
    NewRule {
        targets: Vec<Arc<Block>>,
        deps: Vec<Arc<Block>>,
        double_colon: bool,
        initial_command: Option<AstNode>,
    },

    /// A target specific variable
    TargetVariable {
        targets: Vec<(FileName, Arc<Block>)>,
        variable_action: VariableAction,
    },
}

impl crate::parsers::ParserState {
    pub(crate) fn handle_target_action(
        &mut self,
        engine: &mut Engine,
        action: Action,
    ) -> Result<(), ParseErrorKind> {
        match action {
            Action::NoAction => {
                // Nothing to do
            }
            Action::NewRule {
                targets,
                deps,
                double_colon,
                initial_command,
            } => {
                self.current_rule = Some(ProtoRule {
                    targets,
                    deps,
                    recipe: crate::Recipe(Vec::new()),
                    rule_type: if double_colon {
                        crate::RuleType::DoubleColon
                    } else {
                        crate::RuleType::Standard
                    },
                });

                match initial_command {
                    Some(command) => self.push_command_line(command),
                    None => {}
                }
            }
            Action::TargetVariable {
                targets,
                variable_action,
            } => match variable_action.action {
                variable::Action::Define(parameters) => {
                    for target in targets.into_iter() {
                        engine.database = engine.database.set_variable_for_target(
                            target.0,
                            variable_action.name,
                            parameters.clone(),
                        );
                    }
                }
                variable::Action::Append(node) => {
                    for target in targets.into_iter() {
                        let node = node.clone();
                        engine.database = engine.database.append_to_variable_for_target(
                            target.0,
                            variable_action.name,
                            node.location(),
                            VariableParameters::new(node, Flavor::Recursive, Origin::File),
                            true,
                        )
                    }
                }
            },
        }
        Ok(())
    }
}

/// Represents the reason why we ended the "makefile word"
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum MWordEnd {
    /// End of the line was reached
    EOL,
    /// A static word that can't be expanded was encountered
    Static,
    /// A variable expansion
    Variable,
    /// A colon was encountered
    Colon,
    /// A double colon was encountered
    DColon,
    /// A semicolon was encountered
    Semicolon,
    /// A variable assignment was encountered
    VarAssign,
}

/// This function parses a recipe line. It should be handed a reference to the
/// line, uncollapsed, since we handle collapsing internally
pub(crate) fn parse_line<'a>(
    i: BlockSpan<'a>,
    names: &mut NameCache,
    engine: &mut Engine,
) -> IResult<BlockSpan<'a>, Action, ParseErrorKind> {
    use nom::{InputIter, Slice};

    let line_start = i;
    // TODO: separate line collapse from line acquisition
    let (rest, line) = makefile_line(i, super::ParserCompliance::GNU, false)?;

    // The behavior here in GNU Make is really weird. Why does it consider '$'
    // characters to be semicolons? I'm pretty sure that's the behavior of the
    // implementation in GNU Make but we don't replicate it here because it Causes Problems
    // (~line 1000 in read.c from version 4.2.1).
    //
    // line is the storage backing "line_next" which is our model for lb_next
    // post_semi_content is essentially cmdleft
    let (line, semip) = match makefile_take_until_unquote(line.span(), |c| (c == ';' || c == '#')) {
        (pre, Some((stopchar, post))) => {
            if stopchar.iter_elements().next() == Some('#') {
                (pre, None)
            } else {
                debug!(
                    "Found unescaped semicolon. Post content {:?}",
                    post.into_string()
                );
                (pre, Some(post))
            }
        }
        (pre, None) => (pre, None),
    };
    let mut post_semi_content = semip.map(|x| x.to_new_block());
    debug!(
        "Content after initial semicolon search: {:?}",
        line.into_string()
    );

    macro_rules! expand_segment {
        ($seg:expr) => {{
            let (_, ast) = parse_ast($seg).map_err(|e| lift_collapsed_span_error(e, line_start))?;
            let v = ast.eval(names, engine);
            v
        }};
    }

    let (mut line_next, (mut mw, mut mwt)) =
        next_mword(line.span()).map_err(|e| lift_collapsed_span_error(e, line_start))?;

    match mwt {
        MWordEnd::EOL => {
            if post_semi_content.is_some() {
                return fail_out(line_start, ParseErrorKind::NoRuleRecipe);
            }
            // There was nothing on the line
            return Ok((rest, Action::NoAction));
        }
        MWordEnd::Colon | MWordEnd::DColon => {
            // No targets for this rule, just return immediately
            return Ok((rest, Action::NoAction));
        }
        _ => {}
    }

    // Buffer containing all the expansions we've done so far. This is roughly
    // equivalent to `p2` in the original GNU Make code
    let mut pre_colon_buffer = Block::empty();
    let mut after_colon = None;

    loop {
        debug!(
            "Handling mword {:?} (rest: {:?})",
            mw.into_string(),
            line_next.into_string()
        );
        // This is the content to be pushed to pre_colon_buffer after we finish unquoting it
        let mut new_content = expand_segment!(mw);
        debug!("New content is {:?}", new_content.into_string());

        if post_semi_content.is_none() {
            match makefile_take_until_unquote(new_content.span(), |c| c == ';') {
                (pre, Some((_, post))) => {
                    // We found a semicolon, so expand the rest of the line and
                    // stuff it into post_semi_content.
                    let mut post_content = post.to_new_block();
                    let rest = expand_segment!(line_next);
                    Arc::make_mut(&mut post_content).push_all_contents(rest.span());

                    // We just consumed everything in line_next
                    line_next = line_next.slice(line_next.len()..);

                    // Save what was left after the semicolon
                    post_semi_content = Some(post_content);

                    // The content block is everything before the semicolon
                    new_content = pre;
                }
                (_pre, None) => {
                    // Since there may have been some amount of rewriting from
                    // stripping the escapes, update the expansion buffer
                    // new_content = pre;
                    debug!("No semicolon match");
                }
            }
        }
        debug!(
            "After semicolon search, content is {:?}",
            new_content.into_string()
        );
        if new_content.len() == 0 {
            // Make seems to implicitly consume the whole line in this situation
            return Ok((rest, Action::NoAction));
        }

        match find_non_drivespec_colon(new_content.span()) {
            DrivespecSearchResult::FoundColon {
                pre_colon: pre,
                post_colon: post,
            } => {
                after_colon = Some(post.to_new_block());
                new_content = pre;

                debug!(
                    "Found match, adding prior content {:?} to expanded content",
                    new_content.into_string()
                );
                if pre_colon_buffer.len() > 0 {
                    Arc::make_mut(&mut pre_colon_buffer).push(ContentReference::space());
                }
                Arc::make_mut(&mut pre_colon_buffer).push_all_contents(new_content.span());

                break;
            }
            DrivespecSearchResult::NoColon(pre) => {
                // No match, but we still need to push the content
                debug!("No match for colon search");
                new_content = pre;
            }
        }

        let mword = next_mword(line_next).map_err(|e| lift_collapsed_span_error(e, line_start))?;
        line_next = mword.0;
        mw = (mword.1).0;
        mwt = (mword.1).1;

        if mwt == MWordEnd::EOL {
            break;
        }

        // Push synthetic spaces between spaces
        debug!("Adding {:?} to expanded content", new_content.into_string());
        if pre_colon_buffer.len() > 0 {
            Arc::make_mut(&mut pre_colon_buffer).push(ContentReference::space());
        }
        Arc::make_mut(&mut pre_colon_buffer).push_all_contents(new_content.span());
    }

    debug!("Line split into {:?} and:", pre_colon_buffer.into_string(),);
    debug!("after colon: {:?}", after_colon);
    debug!("after that: {:?}", line_next);

    if mwt == MWordEnd::EOL {
        let (after_space, _) = makefile_whitespace(pre_colon_buffer.span()).unwrap();
        if after_space.len() == 0 {
            // There was only whitespace on the line
            return Ok((rest, Action::NoAction));
        }

        // There was something on the line, but it wasn't what we expected
        return fail_out(line_start, ParseErrorKind::MissingSeparator);
    }

    // Reconstruct the line in different segments

    // Everything before the colon is the list of targets
    let targets =
        crate::parsers::file_sequence::parse_file_seq(pre_colon_buffer.span(), Default::default());
    debug!("Got file sequence {:?}", targets);

    // Everything after the colon gets shoved into one big buffer
    let (post_targets_buffer, pre_semi_len) = {
        let mut block = Block::empty();
        // after_colon must be some, otherwise how did we get here?
        Arc::make_mut(&mut block).push_all_contents(after_colon.unwrap().span());
        // Push whatever is left of the unprocessed line. If line_next is empty,
        // that means there was a semicolon which we handle below
        Arc::make_mut(&mut block).push_all_contents(line_next);
        // Record the length of the block before the synthetic semicolon
        let pre_semi_len = block.len();

        // If we found a semicolon, push a synthetic ';' followed by the post-semicolon content
        match post_semi_content {
            Some(ref content) => {
                Arc::make_mut(&mut block).push(ContentReference::semicolon());
                Arc::make_mut(&mut block).push_all_contents(content.span());
            }
            None => {}
        }

        (block, pre_semi_len)
    };
    let (post_targets_slice, double_colon, pre_semi_len) = {
        let slice = post_targets_buffer.span();
        match tag!(slice, ":") {
            Ok((i, _)) => (i, true, pre_semi_len - 1),
            Err(_) => (slice, false, pre_semi_len),
        }
    };

    // Try to match against a variable assignment operation
    match parse_variable_line(post_targets_slice, names, engine) {
        Ok((_, action)) => {
            let targets = targets
                .into_iter()
                .map(|targ| {
                    let fname = names.intern_file_name(targ.into_string());
                    (fname, targ)
                })
                .collect();
            return Ok((
                rest,
                Action::TargetVariable {
                    targets,
                    variable_action: action,
                },
            ));
        }
        Err(_) => {}
    }

    // Expand the pre-semicolon slice
    let (pre_semi_buffer, post_semi_buffer) = {
        // Split the post_target_slice line back up
        let pre_semi_slice = post_targets_slice.slice(..pre_semi_len);
        let post_semi_slice = if post_targets_slice.len() > pre_semi_len {
            Some(post_targets_slice.slice((pre_semi_len + 1)..))
        } else {
            None
        };

        let pre_semi_buffer = expand_segment!(pre_semi_slice);

        if post_semi_slice.is_none() {
            // Last ditch attempt to find a semicolon
            match makefile_take_until_unquote(pre_semi_buffer.span(), |c| c == ';') {
                (pre, Some((_, post))) => (pre, Some(post.to_new_block())),
                (pre, None) => (pre, None),
            }
        } else {
            (pre_semi_buffer, post_semi_slice.map(|x| x.to_new_block()))
        }
    };
    let pre_semi_slice = pre_semi_buffer.span();
    let post_semi_slice = post_semi_buffer.as_ref().map(|x| x.span());

    // Explicitly drop the post-target buffer: it's now semantically invalid
    std::mem::drop(post_targets_buffer);

    // Now we've definitely found the semicolon if there is one, map the content after it into the command slice
    let command = match post_semi_slice {
        Some(content) => {
            let (_, ast) =
                parse_ast(content).map_err(|e| lift_collapsed_span_error(e, line_start))?;
            Some(ast)
        }
        None => None,
    };

    // See if we can find another escaped colon in the expanded targets. That would indicate this is a
    // static pattern rule. We don't makefile_take_until_unquote since any : should already be escaped
    debug!(
        "Checking for second colon index in {:?}",
        pre_semi_slice.into_string()
    );
    let second_colon_idx = {
        let mut colon_idx = pre_semi_slice.len();
        let mut backslash_count = 0;
        for (idx, chr) in pre_semi_slice.iter_indices() {
            if chr == '\\' {
                backslash_count += 1;
            } else if chr == ':' {
                if backslash_count % 2 == 0 {
                    colon_idx = idx;
                    break;
                } else {
                    backslash_count = 0;
                }
            } else {
                backslash_count = 0;
            }
        }
        if colon_idx < pre_semi_slice.len() {
            Some(colon_idx)
        } else {
            None
        }
    };

    if second_colon_idx.is_some() {
        // check how p is handled ~ line 1255 and how it's computed above that
        unimplemented!("Static pattern rules");
    }

    debug!("Parsing deps from {:?}", pre_semi_slice.into_string());
    let deps = crate::parsers::file_sequence::parse_file_seq(
        pre_semi_slice,
        crate::parsers::file_sequence::FileSeqParseOptions {
            extra_stopchars: ":",
            ..Default::default()
        },
    );

    Ok((
        rest,
        Action::NewRule {
            targets,
            deps,
            double_colon,
            initial_command: command,
        },
    ))
}

fn next_mword<'a>(
    i: BlockSpan<'a>,
) -> IResult<BlockSpan<'a>, (BlockSpan<'a>, MWordEnd), ParseErrorKind> {
    use nom::{InputIter, InputTake};
    // Skip any leading whitespace
    let (i, _) = makefile_whitespace(i)?;

    // Early out on simple cases
    if let Ok((i, (cap, end_reason))) = pe_fix!(
        i,
        alt!(
            tag!(";") =>   { |c| (c, MWordEnd::Semicolon) } |
            tag!("=") =>   { |c| (c, MWordEnd::VarAssign) } |
            tag!(":=") =>  { |c| (c, MWordEnd::VarAssign) } |
            tag!("::=") => { |c| (c, MWordEnd::VarAssign) } |
            tag!("::") =>  { |c| (c, MWordEnd::DColon) } |
            tag!(":") =>   { |c| (c, MWordEnd::Colon) } |
            tag!("+=") =>  { |c| (c, MWordEnd::VarAssign) } |
            tag!("?=") =>  { |c| (c, MWordEnd::VarAssign) } |
            tag!("!=") =>  { |c| (c, MWordEnd::VarAssign) }
        )
    ) {
        return Ok((i, (cap, end_reason)));
    };

    // More complex cases
    let mut char_iter = i.iter_indices().peekable();
    let mut stop_index = 0;
    let mut end_reason = MWordEnd::Static;
    let mut prev_char = '\0';

    while let Some((idx, chr)) = char_iter.next() {
        debug!("inspect {:?} at {:?}", chr, idx);
        stop_index = idx + 1;

        if chr == ' ' || chr == '\t' || chr == '=' {
            end_reason = MWordEnd::Static;
            stop_index = idx;
            break;
        }

        if chr == ':' {
            if char_iter.peek() == Some(&(idx + 1, '\\')) && prev_char.is_alphabetic() {
                debug!("Skipping DOS path {:?}", idx);
                continue;
            }
            stop_index = idx + 1;
            break;
        }

        if chr == '$' {
            // This is probably a variable reference of some sort
            if let Some((idx, chr)) = char_iter.next() {
                stop_index = idx + 1;
                if chr == '$' {
                    // Consume the character
                    // This is just a double '$', does not end the word
                    prev_char = chr;
                    continue;
                }
                end_reason = MWordEnd::Variable;

                // Scan forward until we find the matching end brace
                let brace_end = if chr == '(' {
                    ')'
                } else if chr == '{' {
                    '}'
                } else {
                    // Single character variable reference
                    prev_char = chr;
                    continue;
                };

                let mut count = 1;
                while let Some((idx, chr2)) = char_iter.next() {
                    stop_index = idx + 1;
                    if chr2 == chr {
                        count += 1;
                    } else if chr2 == brace_end {
                        count -= 1;
                    }
                    if count == 0 {
                        break;
                    }
                }
                prev_char = brace_end;
                continue;
            } else {
                end_reason = MWordEnd::Static;
                break;
            }
        }

        if chr == '\\' {
            prev_char = chr;
            if let Some((_, chr)) = char_iter.peek() {
                let chr = *chr;
                if chr == ':' || chr == ';' || chr == '=' || chr == '\\' {
                    // This is escaping something, skip forward
                    prev_char = chr;
                    char_iter.next();
                }
            }
            continue;
        }

        if chr == '?' || chr == '+' {
            // If we see an assignment-like thing, the word is over
            if let Some((_, chr)) = char_iter.peek() {
                // This subtraction is safe since if we were on the first character,
                // the early-out above would have caught the "?="
                stop_index -= 1;
                if *chr == '=' {
                    break;
                }
            }
        }
        prev_char = chr;
    }

    let (i, cap) = i.take_split(stop_index);
    debug!("Captured content {:?}", cap.into_string());

    Ok((i, (cap, end_reason)))
}

enum DrivespecSearchResult<'a> {
    NoColon(Arc<Block>),
    FoundColon {
        pre_colon: Arc<Block>,
        post_colon: BlockSpan<'a>,
    },
}

/// This whole function implements lines 1087 to 1094 in read.c, make version
/// 4.2.1. Our life is harder because we want to actually track origins of text
/// blocks and can't just do cowboy pointer arithmetic, but in general it should
/// do the same thing (run take_until_unquote until we find an unquoted : that
/// isn't a drive spec)
/// We consider something to be a drivespec if the following conditions hold:
///  1. The character before the colon is an alphabetic character
///  2. The character before that is (a) whitespace, or an open paren, or (b) the beginning of the line
///  3. The character after the colon is `\` or `/`.
fn find_non_drivespec_colon<'a>(mut i: BlockSpan<'a>) -> DrivespecSearchResult<'a> {
    use nom::{InputIter, Slice};

    let mut content = Block::empty();
    loop {
        match makefile_take_until_unquote(i, |c| c == ':') {
            (pre, Some((colon, post))) => {
                // We definitely are going to want the pre-content to be pushed into the output content
                Arc::make_mut(&mut content).push_all_contents(pre.span());

                // Now check to see if this is a drivespec. If any of the conditions fail, immediately return
                if let Some(ch) = post.iter_elements().next() {
                    if ch != '/' && ch != '\\' {
                        // condition (3) doesn't hold, this isn't a drivespec
                        return DrivespecSearchResult::FoundColon {
                            pre_colon: content,
                            post_colon: post,
                        };
                    }
                } else {
                    // Post was empty, this can't be a drivespec (failed (3))
                    return DrivespecSearchResult::FoundColon {
                        pre_colon: content,
                        post_colon: post,
                    };
                }
                // Checking condition (1) and (2) requires slicing
                if pre.len() == 0 {
                    // Condition 2 fails automatically since there was no prior content
                    return DrivespecSearchResult::FoundColon {
                        pre_colon: content,
                        post_colon: post,
                    };
                } else if pre.len() == 1 {
                    // Condition 2b holds, check condition 1
                    let ch = pre.span().iter_elements().next().unwrap();
                    if !ch.is_alphabetic() {
                        return DrivespecSearchResult::FoundColon {
                            pre_colon: content,
                            post_colon: post,
                        };
                    }
                } else {
                    // There were at least 2 prior characters, grab them
                    let pre_slice = pre.span().slice(pre.len() - 2..);
                    assert!(pre_slice.len() == 2);
                    let (ch1, ch2) = {
                        let mut iter = pre_slice.iter_elements();
                        (iter.next().unwrap(), iter.next().unwrap())
                    };
                    // Check condition 1 and 2
                    if !ch1.is_alphabetic() || !(ch2.is_whitespace() || ch2 == '(') {
                        // Condition 1 or 2 failed, not a drive spec
                        return DrivespecSearchResult::FoundColon {
                            pre_colon: content,
                            post_colon: post,
                        };
                    }
                }

                // This colon passed all checks and is thus a drivespecc. Push
                // the colon itself into content and continue searching
                Arc::make_mut(&mut content).push_all_contents(colon);

                i = post;
            }
            (pre, None) => {
                // Colon was not found at all, immediate return
                Arc::make_mut(&mut content).push_all_contents(pre.span());
                return DrivespecSearchResult::NoColon(content);
            }
        };
    }
}

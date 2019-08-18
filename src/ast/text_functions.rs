//! implementation of all the text-munging functions ()

use crate::evaluated::{Block, BlockSpan, ContentReference};
use crate::parsers::{makefile_take_until_unquote, makefile_token};
use crate::types::Set;
use crate::{Engine, VariableName};
use nom::{InputIter, Slice};
use std::sync::Arc;

/// Parses a variable value, and returns the root of the combined tree node
pub(super) fn do_subref(
    sensitivity: Set<VariableName>,
    variable_value: Arc<Block>,
    key: Arc<Block>,
    replacement: Arc<Block>,
) -> Arc<Block> {
    match makefile_take_until_unquote(key.span(), |ch| ch == '%') {
        (pre_key, Some((_, post_key))) => {
            match makefile_take_until_unquote(replacement.span(), |ch| ch == '%') {
                // Both key and replacement had a %
                (pre_replacement, Some((_, post_replacement))) => do_replacement(
                    variable_value.span(),
                    sensitivity,
                    &pre_key.into_string(),
                    &post_key.into_string(),
                    pre_replacement.span(),
                    post_replacement,
                ),
                // Only key had a %
                (replacement, None) => do_replacement(
                    variable_value.span(),
                    sensitivity,
                    &pre_key.into_string(),
                    &post_key.into_string(),
                    BlockSpan::empty(),
                    replacement.span(),
                ),
            }
        }
        (key, None) => do_replacement(
            variable_value.span(),
            sensitivity,
            "",
            &key.into_string(),
            BlockSpan::empty(),
            replacement.span(),
        ),
    }
}

/// Internal function that can actually do replacement
fn do_replacement<'a>(
    mut tokens: BlockSpan,
    sensitivity: Set<VariableName>,
    pre_key: &str,
    post_key: &str,
    pre_replacement: BlockSpan,
    post_replacement: BlockSpan,
) -> Arc<Block> {
    let mut output = Block::new(sensitivity, Default::default());
    let total_key_length = pre_key.len() + post_key.len();
    let mut first = true;
    while tokens.len() > 0 {
        let (new_tokens, token) = makefile_token(tokens).expect("tokens should never fail");
        tokens = new_tokens;

        if !first {
            // We just pushed something else, so push a space
            Arc::make_mut(&mut output).push(crate::evaluated::ContentReference::space());
        } else {
            first = false;
        }

        debug!("processing token {:?}", token.into_string());

        if token.len() < total_key_length {
            // The token is too short to match the key, just push all its contents unmodified
            debug!("Skipping because too short");
            Arc::make_mut(&mut output).push_all_contents(token);
            continue;
        }

        let (token_remaining, captured) = match delimited!(
            token,
            tag!(pre_key),
            recognize!(many1!(take_until!(post_key))),
            tag!(post_key)
        ) {
            Ok(v) => v,
            Err(_) => {
                // A failure to match here indicates we couldn't find the key,
                // so just push everything and move on
                debug!("Skipping because capture failed");
                Arc::make_mut(&mut output).push_all_contents(token);
                continue;
            }
        };
        if token_remaining.len() != 0 {
            // We didn't make it to the end of the line when matching for the post key,
            // so the key doesn't match. Push and continue
            debug!(
                "Skipping because capture was not completed {:?}",
                token_remaining.into_string()
            );
            Arc::make_mut(&mut output).push_all_contents(token);
            continue;
        }
        // Push the replacement
        Arc::make_mut(&mut output).push_all_contents(pre_replacement.clone());
        Arc::make_mut(&mut output).push_all_contents(captured.clone());
        Arc::make_mut(&mut output).push_all_contents(post_replacement.clone());
    }
    // Since we just pushed a bunch of garbage we should be nice and simplify
    // the block before returning it
    Arc::make_mut(&mut output).simplify();

    output
}

pub(super) fn abspath(input: BlockSpan, engine: &Engine) -> Arc<Block> {
    let prefix = {
        use crate::evaluated::nodes::EvaluatedNode;
        use crate::source_location::LocatedString;

        let mut p = engine.working_directory.to_string_lossy().into_owned();
        p.push(std::path::MAIN_SEPARATOR);

        Arc::new(EvaluatedNode::Constant(LocatedString::synthetic_new(p)))
    };

    let mut output_content = Vec::new();
    let mut push_space = false;

    for item in super::utils::by_make_token(input) {
        debug!("abspathing {:?}", item.into_string());
        if push_space {
            output_content.push(ContentReference::space());
        }

        if item.len() == 0 {
            // TODO: this should report a file location or something like that
            warn!(
                "Skipping zero-length content from {:?}",
                input.into_string()
            );
            push_space = false;
            continue;
        }

        output_content.push(ContentReference::new_from_node(prefix.clone()));
        output_content.push(item.to_content_reference());

        push_space = true;
    }

    Block::new(input.parent().raw_sensitivity().clone(), output_content)
}

pub(super) fn strip(input: BlockSpan) -> Arc<Block> {
    let mut start = 0;
    let mut last_non_whitespace = 0;

    let mut it = input.iter_indices();

    while let Some((idx, chr)) = it.next() {
        if !chr.is_whitespace() {
            last_non_whitespace = idx + chr.len_utf8();
            break;
        }
        start = idx;
    }

    for (idx, chr) in it {
        if !chr.is_whitespace() {
            last_non_whitespace = idx + chr.len_utf8();
        }
    }

    Block::new(
        input.parent().raw_sensitivity().clone(),
        vec![input
            .slice(start..last_non_whitespace)
            .to_content_reference()],
    )
}


pub(super) fn firstword(input: BlockSpan) -> Arc<Block> {
    let mut output_content = Vec::new();

    if let Some(block) = super::utils::by_make_token(input).next() {
        output_content.push(block.to_content_reference());
    }

    Block::new(input.parent().raw_sensitivity().clone(), output_content)
}

pub(super) fn findstring(needle: BlockSpan, haystack: BlockSpan) -> Arc<Block> {
    use nom::FindSubstring;

    let sensitivity = needle.parent().raw_sensitivity().clone();
    let sensitivity = sensitivity.union(haystack.parent().raw_sensitivity().clone());

    let mut output_content = Vec::new();
    let needle = needle.into_string();

    if let Some(i) = haystack.find_substring(&needle) {
        output_content.push(haystack.slice(i..(i + needle.len())).to_content_reference());
    }

    Block::new(sensitivity, output_content)
}

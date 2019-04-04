//! implementation of all the text-munging functions ()

use crate::evaluated::{Block, BlockSpan};
use crate::parsers::{makefile_take_until_unquote, makefile_token};
use crate::types::Set;
use crate::VariableName;
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

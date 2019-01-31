//! implementation of all the text-munging functions ()

use crate::evaluated::{Block, BlockSpan};
use crate::parsers::{makefile_take_until_unquote, makefile_token};
use crate::VariableName;
use fxhash::FxHashSet;
use std::sync::Arc;

/// Parses a variable value, and returns the root of the combined tree node
pub(super) fn do_subref(
    sensitivity: FxHashSet<VariableName>,
    variable_value: Arc<Block>,
    key: Arc<Block>,
    replacement: Arc<Block>,
) -> Arc<Block> {
    match makefile_take_until_unquote(key.span(), '%') {
        Ok((post_key, pre_key)) => match makefile_take_until_unquote(replacement.span(), '%') {
            // Both key and replacement had a %
            Ok((post_replacement, pre_replacement)) => do_replacement(
                variable_value.span(),
                sensitivity,
                &pre_key.into_string(),
                &post_key.into_string(),
                pre_replacement.span(),
                post_replacement,
            ),
            // Only key had a %
            Err(_) => do_replacement(
                variable_value.span(),
                sensitivity,
                &pre_key.into_string(),
                &post_key.into_string(),
                BlockSpan::empty(),
                replacement.span(),
            ),
        },
        Err(_) => do_replacement(
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
    sensitivity: FxHashSet<VariableName>,
    pre_key: &str,
    post_key: &str,
    pre_replacement: BlockSpan,
    post_replacement: BlockSpan,
) -> Arc<Block> {
    let mut output = Block::new(sensitivity, Default::default());
    let total_key_length = pre_key.len() + post_key.len();
    let mut first = true;
    while tokens.len() > 0 {
        use nom::Slice;

        let (new_tokens, token) = makefile_token(tokens).expect("tokens should never fail");
        tokens = new_tokens;

        if !first {
            // We just pushed something else, so push a space
            Arc::make_mut(&mut output).push(crate::evaluated::ContentReference::space());
        } else {
            first = false;
        }

        eprintln!("processing token {:?}", token.into_string());

        if token.len() < total_key_length {
            // The token is too short to match the key, just push all its contents unmodified
            eprintln!("Skipping because too short");
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
                eprintln!("Skipping because capture failed");
                Arc::make_mut(&mut output).push_all_contents(token);
                continue;
            }
        };
        if token_remaining.len() != 0 {
            // We didn't make it to the end of the line when matching for the post key,
            // so the key doesn't match. Push and continue
            eprintln!(
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

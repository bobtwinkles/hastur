//! The result of evaluating an AST
use crate::tokenizer::BuiltinFunction;
use crate::types::Set;
use crate::VariableName;
use arrayvec::ArrayVec;
use std::sync::Arc;

#[cfg(test)]
pub(crate) mod test;

pub mod block_span;
pub mod content_reference;
pub mod nodes;
mod nom;

// Reexport of the node interface
pub use self::block_span::BlockSpan;
pub use self::content_reference::ContentReference;
pub use self::nodes::*;

/// A fully evaluated block of text
#[derive(Clone, Default, Debug, PartialEq)]
pub struct Block {
    /// What variables this evaluation was sensitive to
    // #REQ-Expansion-Tracking
    sensitivity: Set<VariableName>,
    /// The different fragments of content, tagged with how they came to be part of this block
    // #SPC-Variable-Eval.concat
    content: Vec<ContentReference>,
}

lazy_static::lazy_static!(
    static ref EMPTY_BLOCK: Arc<Block> = Default::default();
);

impl Block {
    /// An empty block
    pub(crate) fn empty() -> Arc<Block> {
        EMPTY_BLOCK.clone()
    }

    /// Create a new evaluated block
    pub(crate) fn new(
        sensitivity: Set<VariableName>,
        content: Vec<ContentReference>,
    ) -> Arc<Block> {
        let mut tr_block = Block {
            sensitivity,
            content,
        };
        tr_block.simplify();
        Arc::new(tr_block)
    }

    /// Flatten all the leaves into a single string.
    /// This discards all semantic information, so it's mostly useful for
    /// debugging and unit test assertions. If you want to retain semantic
    /// information, consider using one of visitor methods instead.
    pub fn into_string(&self) -> String {
        let mut buffer = String::new();
        for reference in &self.content {
            reference.append_to_string(&mut buffer);
        }
        buffer
    }

    /// Get the total length of this block in bytes
    pub fn len(&self) -> usize {
        self.content.iter().map(|x| x.length).sum()
    }

    /// Get this block as a parsable span
    pub fn span(&self) -> BlockSpan {
        let length = self.content.iter().map(|x| x.length).sum();
        BlockSpan {
            parent: self,
            contents: &self.content,
            offset: 0,
            length: length,
        }
    }

    /// Get a list of things that this tree is sensitive too
    pub fn sensitivity(&self) -> impl Iterator<Item = &VariableName> {
        self.sensitivity.iter()
    }

    /// Get a copy of the raw sensitivity set
    pub(crate) fn raw_sensitivity(&self) -> Set<VariableName> {
        self.sensitivity.clone()
    }

    /// Consume this tree, leaving behind an iterator over its sensitivities
    pub fn into_sensitivity(self) -> impl Iterator<Item = VariableName> {
        self.sensitivity.into_iter()
    }

    /// Push more content into this block
    pub fn push(&mut self, content: ContentReference) {
        self.content.push(content);
    }

    /// Push all the contents from a span into this block
    pub fn push_all_contents(&mut self, span: BlockSpan) {
        if span.length == 0 {
            return;
        }

        // Add all the content
        assert!(span.contents.len() > 0);
        assert!(span.offset < span.contents[0].length);

        let mut contents_iter = span.contents.iter().map(|x| x.clone());
        // Unwrap is safe since we asserted above that contents length was > 0
        let mut to_push = contents_iter.next().unwrap();
        to_push.offset += span.offset;
        to_push.length -= span.offset;

        let mut remaining_length = span.length;

        while to_push.length < remaining_length {
            remaining_length -= to_push.length;
            self.content.push(to_push);
            to_push = contents_iter
                .next()
                .expect("ran out of contents before reaching the expected length");
        }
        assert!(to_push.length >= remaining_length);
        to_push.length = remaining_length;
        self.content.push(to_push);

        // If this span comes from another block, add the sensitivity of that block to this one.
        // We could probably get away with being less sensitive (e.g. if the
        // content that caused the other block to be sensitive to a particular
        // variable isn't actually in the provided span), but for now err on the
        // side of caution
        if !std::ptr::eq(self, span.parent) {
            for v in span.parent.sensitivity.iter() {
                self.sensitivity = self.sensitivity.update(*v);
            }
        }
    }

    /// Iterate over the contents of this block
    pub fn content(&self) -> impl Iterator<Item = &ContentReference> {
        self.content.iter()
    }

    /// Simplify the contents of the block by reducing the size of concats and constants
    pub(crate) fn simplify(&mut self) {
        self.content = self
            .content
            .iter_mut()
            .filter_map(|content| {
                if content.length == 0 {
                    None
                } else {
                    content.simplify();
                    Some(content.clone())
                }
            })
            .collect();
    }
}

/// Create a content reference to a constant
pub fn constant(contents: crate::source_location::LocatedString) -> ContentReference {
    ContentReference::new_from_node(Arc::new(EvaluatedNode::Constant(contents)))
}

/// Create a content reference to a concatenation
pub fn concat(sensitivity: Set<VariableName>, contents: Vec<ContentReference>) -> ContentReference {
    ContentReference::new_from_node(Arc::new(EvaluatedNode::Concat(Block::new(
        sensitivity,
        contents,
    ))))
}

/// Create a content reference to a variable reference
pub fn variable_reference(name: Arc<Block>, value: Arc<Block>) -> ContentReference {
    ContentReference::new_from_node(Arc::new(EvaluatedNode::VariableReference(
        nodes::VariableReference::new(name, value),
    )))
}

/// Create a content reference to a variable substitution reference
pub fn substitution_reference(
    name: Arc<Block>,
    key: Arc<Block>,
    replacement: Arc<Block>,
    value: Arc<Block>,
) -> ContentReference {
    ContentReference::new_from_node(Arc::new(EvaluatedNode::SubstitutionReference(
        nodes::SubstitutionReference::new(name, key, replacement, value),
    )))
}

macro_rules! evaluated_function {
    ($(#[$attr:meta])* $fname:ident, $func:ident, $( $i:ident ),+) => {
        evaluated_function!($(#[$attr])* $fname, stringify!($fname), $func, $($i),*);
    };
    ($(#[$attr:meta])* $fname:ident, $fname_str:expr, $func:ident, $( $i:ident ),+) => {
        #[doc = "The result of evaluating the `"]
        #[doc = $fname_str]
        #[doc = "` function"]
        $(#[$attr])*
        pub fn $fname($($i: Arc<Block>),+ , output: Arc<Block>) -> ContentReference {
            let mut args = ArrayVec::new();
            $(
                args.push($i);
            );+
                ContentReference::new_from_node(Arc::new(EvaluatedNode::FunctionEvaluation(Box::new(
                    nodes::FunctionEvaluation::new(
                        BuiltinFunction::$func, args, output
                    )))))
        }
    };
}

evaluated_function!(abspath, Abspath, input);
evaluated_function!(firstword, FirstWord, input);
evaluated_function!(findstring, FindString, needle, haystack);
evaluated_function!(if_three_args, If, condition, true_case, false_case);
evaluated_function!(if_two_args, If, condition, true_case);
evaluated_function!(strip, Strip, input);
evaluated_function!(shell, Shell, input);

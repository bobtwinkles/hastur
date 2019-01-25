//! The result of evaluating an AST
use crate::VariableName;
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
    sensitivity: fxhash::FxHashSet<VariableName>,
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
        sensitivity: fxhash::FxHashSet<VariableName>,
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

    /// Consume this tree, leaving behind an iterator over its sensitivities
    pub fn into_sensitivity(self) -> impl Iterator<Item = VariableName> {
        self.sensitivity.into_iter()
    }

    /// Push more content into this block
    pub fn push(&mut self, content: ContentReference) {
        self.content.push(content);
    }

    /// Iterate over the contents of this block
    pub fn content(&self) -> impl Iterator<Item = &ContentReference> {
        self.content.iter()
    }

    /// Simplify the contents of the block by reducing the size of concats and constants
    fn simplify(&mut self) {
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
pub fn concat(contents: Arc<Block>) -> ContentReference {
    ContentReference::new_from_node(Arc::new(EvaluatedNode::Concat(contents)))
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

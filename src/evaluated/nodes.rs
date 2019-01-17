//! All the nodes

use super::{Block, BlockSpanIter};
use crate::source_location::LocatedString;
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq)]
pub enum EvaluatedNode {
    /// Represents a constant block of text
    // #SPC-Variable-Eval.constant
    Constant(LocatedString),
    /// The concatenation of some other nodes,
    Concat(Vec<Arc<Block>>),
    /// A reference to a variable
    VariableReference(Box<VariableReference>),
    /// A reference to a variable that applies some substitution
    SubstitutionReference(Box<SubstitutionReference>),
}

impl EvaluatedNode {
    /// Get the total length of content
    pub fn len(&self) -> usize {
        match self {
            EvaluatedNode::Constant(s) => s.len(),
            EvaluatedNode::Concat(v) => v.iter().map(|x| x.len()).sum(),
            EvaluatedNode::VariableReference(v) => v.value.len(),
            EvaluatedNode::SubstitutionReference(v) => v.value.len(),
        }
    }

    /// Iterate over the characters in this nodes
    pub fn chars(&self) -> Chars {
        use nom::InputIter;
        match self {
            EvaluatedNode::Constant(v) => Chars(CharsInternal::Constant(v.chars())),
            EvaluatedNode::Concat(values) => {
                let mut block_iter = values.iter();
                let span_iter = match block_iter.next() {
                    Some(x) => x,
                    None => return Chars(CharsInternal::Constant("".chars())),
                }.span().chars();
                Chars(CharsInternal::Concat {
                    block_iter,
                    span_iter,
                })
            }
            EvaluatedNode::VariableReference(val) => Chars(CharsInternal::VariableReference(
                val.value.span().iter_elements(),
            )),
            EvaluatedNode::SubstitutionReference(val) => Chars(
                CharsInternal::SubstitutionReference(val.value.span().iter_elements()),
            ),
        }
    }
}

/// Iterator over the characters in an evaluated node
#[derive(Clone, Debug)]
pub struct Chars<'a>(CharsInternal<'a>);

impl<'a> Iterator for Chars<'a> {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        self.0.next()
    }
}

/// Implementation of the character iterator
#[derive(Clone, Debug)]
enum CharsInternal<'a> {
    Constant(std::str::Chars<'a>),
    Concat {
        block_iter: std::slice::Iter<'a, Arc<Block>>,
        span_iter: BlockSpanIter<'a>,
    },
    VariableReference(BlockSpanIter<'a>),
    SubstitutionReference(BlockSpanIter<'a>),
}

impl<'a> Iterator for CharsInternal<'a> {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        match self {
            CharsInternal::Constant(it) => it.next(),
            CharsInternal::Concat {
                ref mut block_iter,
                ref mut span_iter,
            } => {
                let mut tr = span_iter.next();
                while tr == None {
                    *span_iter = block_iter.next()?.span().chars();
                    tr = span_iter.next();
                }
                tr
            }
            CharsInternal::VariableReference(it) => it.next(),
            CharsInternal::SubstitutionReference(it) => it.next(),
        }
    }
}

/// A reference to a variable
// #SPC-Variable-Eval.variable_reference
#[derive(Clone, Debug, PartialEq)]
pub struct VariableReference {
    name: Arc<Block>,
    value: Arc<Block>,
}

impl VariableReference {
    /// Create a new variable reference.
    /// You shouldn't need to do this manually if your primarily use of Hastur
    /// is parsing existing makefiles.
    pub fn new(name: Arc<Block>, value: Arc<Block>) -> Box<Self> {
        Box::new(VariableReference { name, value })
    }

    /// Get the evaluated version of the name of this variable
    pub fn name(&self) -> &Block {
        &self.name
    }
}

/// A reference to a variable that applies some substitution
// #SPC-Variable-Eval.substitution_reference
#[derive(Clone, Debug, PartialEq)]
pub struct SubstitutionReference {
    name: Arc<Block>,
    key: Arc<Block>,
    replacement: Arc<Block>,
    value: Arc<Block>,
}

impl SubstitutionReference {
    pub fn new(
        name: Arc<Block>,
        key: Arc<Block>,
        replacement: Arc<Block>,
        value: Arc<Block>,
    ) -> Box<Self> {
        Box::new(Self {
            name,
            key,
            replacement,
            value,
        })
    }

    /// Get the name of the variable that was referenced to construct this node
    pub fn name(&self) -> &Block {
        &self.name
    }

    /// Get the key used for replacement
    pub fn key(&self) -> &Block {
        &self.key
    }

    /// Get the replacement for the key
    pub fn replacement(&self) -> &Block {
        &self.replacement
    }

    /// Get the value produced when this reference was evaluated.
    pub fn value(&self) -> &Block {
        &self.value
    }
}

//! All the nodes

use super::block_span::Iter;
use super::Block;
use crate::source_location::{LocatedString, Location};
use std::sync::Arc;

/// Represents a node in the expansion history of some text.
#[derive(Debug, Clone, PartialEq)]
pub enum EvaluatedNode {
    /// Represents a constant block of text
    // #SPC-Variable-Eval.constant
    Constant(LocatedString),
    /// The concatenation of some other nodes, collapsed into a single block
    Concat(Arc<Block>),
    /// A reference to a variable
    VariableReference(Box<VariableReference>),
    /// A reference to a variable that applies some substitution
    SubstitutionReference(Box<SubstitutionReference>),
    /// A reference to content produced by an eval function call
    Evaluated(Box<Evaluated>),
    /// A reference to content produced by a `abspath` function call
    Abspath(Box<Abspath>),
    /// A reference to content produced by a `firstword` function call
    FirstWord(Box<FirstWord>),
    /// A reference to content produced by a `strip` function call
    Strip(Box<Strip>),
}

lazy_static::lazy_static!(
    static ref WHITESPACE: Arc<EvaluatedNode> = Arc::new(
        EvaluatedNode::Constant(LocatedString::new(Location::Synthetic.into(), " ".into()))
    );
    static ref NEWLINE: Arc<EvaluatedNode> = Arc::new(
        EvaluatedNode::Constant(LocatedString::new(Location::Synthetic.into(), "\n".into()))
    );
    static ref SEMICOLON: Arc<EvaluatedNode> = Arc::new(
        EvaluatedNode::Constant(LocatedString::new(Location::Synthetic.into(), ";".into()))
    );
);

impl EvaluatedNode {
    /// Get a new reference to the space token
    pub(crate) fn space() -> Arc<EvaluatedNode> {
        Arc::clone(&WHITESPACE)
    }

    /// Get a reference to the newline token
    pub(crate) fn newline() -> Arc<EvaluatedNode> {
        Arc::clone(&NEWLINE)
    }

    /// Get a new reference to the semicolon token
    pub(crate) fn semicolon() -> Arc<EvaluatedNode> {
        Arc::clone(&SEMICOLON)
    }

    /// Get the total length of content
    pub fn len(&self) -> usize {
        match self {
            EvaluatedNode::Constant(s) => s.len(),
            EvaluatedNode::Concat(v) => v.len(),
            EvaluatedNode::VariableReference(v) => v.value.len(),
            EvaluatedNode::SubstitutionReference(v) => v.value.len(),
            EvaluatedNode::Evaluated(v) => v.value.len(),
            EvaluatedNode::Abspath(v) => v.output.len(),
            EvaluatedNode::FirstWord(v) => v.output.len(),
            EvaluatedNode::Strip(v) => v.output.len(),
        }
    }

    /// Iterate over the characters in this nodes
    pub fn chars(&self) -> Chars {
        match self {
            EvaluatedNode::Constant(v) => Chars(CharsInternal::Direct(v.chars())),
            EvaluatedNode::Concat(v) => Chars(CharsInternal::BlockSpan(v.span().chars())),
            EvaluatedNode::VariableReference(val) => {
                Chars(CharsInternal::BlockSpan(val.value.span().chars()))
            }
            EvaluatedNode::SubstitutionReference(val) => {
                Chars(CharsInternal::BlockSpan(val.value.span().chars()))
            }
            EvaluatedNode::Evaluated(v) => Chars(CharsInternal::BlockSpan(v.value.span().chars())),
            EvaluatedNode::Abspath(v) => Chars(CharsInternal::BlockSpan(v.output.span().chars())),
            EvaluatedNode::FirstWord(v) => Chars(CharsInternal::BlockSpan(v.output.span().chars())),
            EvaluatedNode::Strip(v) => Chars(CharsInternal::BlockSpan(v.output.span().chars())),
        }
    }

    /// Iterate over the segments in this node
    pub fn segments(&self) -> SegmentsIter {
        SegmentsIter(match self {
            EvaluatedNode::Constant(v) => SegmentsInternal::OneShot(v.as_str()),
            EvaluatedNode::Concat(v) => SegmentsInternal::BlockSpan(v.span().segments()),
            EvaluatedNode::VariableReference(v) => {
                SegmentsInternal::BlockSpan(v.value.span().segments())
            }
            EvaluatedNode::SubstitutionReference(v) => {
                SegmentsInternal::BlockSpan(v.value.span().segments())
            }
            EvaluatedNode::Evaluated(v) => SegmentsInternal::BlockSpan(v.value.span().segments()),
            EvaluatedNode::Abspath(v) => SegmentsInternal::BlockSpan(v.output.span().segments()),
            EvaluatedNode::FirstWord(v) => SegmentsInternal::BlockSpan(v.output.span().segments()),
            EvaluatedNode::Strip(v) => SegmentsInternal::BlockSpan(v.output.span().segments()),
        })
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
    Direct(std::str::Chars<'a>),
    BlockSpan(Iter<'a>),
}

impl<'a> Iterator for CharsInternal<'a> {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        match self {
            CharsInternal::Direct(it) => it.next(),
            CharsInternal::BlockSpan(it) => it.next(),
        }
    }
}

/// An iterator over the segments in this block
#[derive(Clone, Debug)]
pub struct SegmentsIter<'a>(SegmentsInternal<'a>);

/// Implementation of the segments iterator
#[derive(Clone, Debug)]
enum SegmentsInternal<'a> {
    OneShot(crate::source_location::LocatedStr<'a>),
    BlockSpan(super::block_span::SegmentsIter<'a>),
    Exhausted,
}

impl<'a> Iterator for SegmentsIter<'a> {
    type Item = crate::source_location::LocatedStr<'a>;

    fn next(&mut self) -> Option<crate::source_location::LocatedStr<'a>> {
        let tr = match self.0 {
            SegmentsInternal::OneShot(ref c) => Some(c.clone()),
            SegmentsInternal::BlockSpan(ref mut it) => it.next(),
            SegmentsInternal::Exhausted => None,
        };
        if let SegmentsInternal::OneShot(_) = self.0 {
            self.0 = SegmentsInternal::Exhausted;
        }
        tr
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
    /// Create a new substitution reference
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

/// Content that came out of an eval block
#[derive(Clone, Debug, PartialEq)]
pub struct Evaluated {
    value: Arc<Block>,
}

impl Evaluated {
    /// Create a new Evaluated node
    /// TODO: should this take a Database as context?
    pub fn new(value: Arc<Block>) -> Box<Self> {
        Box::new(Self { value })
    }

    /// Get the value produced
    pub fn value(&self) -> &Block {
        &self.value
    }
}

/// Content that came out of an abspath block
#[derive(Clone, Debug, PartialEq)]
pub struct Abspath {
    input: Arc<Block>,
    output: Arc<Block>,
}

impl Abspath {
    /// Create a new Evaluated node
    /// TODO: should this take a Database as context?
    pub fn new(input: Arc<Block>, output: Arc<Block>) -> Box<Self> {
        Box::new(Self { input, output })
    }

    /// Get the value consumed
    pub fn input(&self) -> &Block {
        &self.input
    }

    /// Get the value produced
    pub fn output(&self) -> &Block {
        &self.output
    }
}

/// Content that came out of an firstword block
#[derive(Clone, Debug, PartialEq)]
pub struct FirstWord {
    input: Arc<Block>,
    output: Arc<Block>,
}

impl FirstWord {
    /// Create a new Evaluated node
    /// TODO: should this take a Database as context?
    pub fn new(input: Arc<Block>, output: Arc<Block>) -> Box<Self> {
        Box::new(Self { input, output })
    }

    /// Get the value consumed
    pub fn input(&self) -> &Block {
        &self.input
    }

    /// Get the value produced
    pub fn output(&self) -> &Block {
        &self.output
    }
}

/// Content that came out of an strip block
#[derive(Clone, Debug, PartialEq)]
pub struct Strip {
    input: Arc<Block>,
    output: Arc<Block>,
}

impl Strip {
    /// Create a new Evaluated node
    /// TODO: should this take a Database as context?
    pub fn new(input: Arc<Block>, output: Arc<Block>) -> Box<Self> {
        Box::new(Self { input, output })
    }

    /// Get the value consumed
    pub fn input(&self) -> &Block {
        &self.input
    }

    /// Get the value produced
    pub fn output(&self) -> &Block {
        &self.output
    }
}

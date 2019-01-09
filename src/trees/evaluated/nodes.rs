//! All the nodes

use super::{EvaluatedTree, EvaluatedNode, EvaluatedPrivate};
use crate::source_location::OwnedStringSpan;
use std::any::Any;

/// Implements functions from the private interface which have to be copy/pasted
macro_rules! partial_private_impls {
    ($node:tt) => {
        fn dynamic_eq(&self, o: &dyn EvaluatedNode) -> bool {
            let o = match o.as_any().downcast_ref::<Self>() {
                Some(x) => x,
                None => return false,
            };
            self == o
        }
    };
}

/// Implements functions from the public interface which have to be copy/pasted
macro_rules! partial_node_impls {
    ($node:tt) => {
        fn as_any(&self) -> &(Any + 'static) {
            self
        }

        fn as_private(&self) -> &(dyn EvaluatedPrivate + 'static) {
            self
        }
    };
}

/// Represents a constant block of text
// #SPC-Variable-Eval.constant
#[derive(Clone, Debug, PartialEq)]
pub struct Constant {
    contents: OwnedStringSpan,
}

impl Constant {
    /// Get the internal span
    pub fn span(&self) -> &OwnedStringSpan {
        &self.contents
    }
}

impl EvaluatedPrivate for Constant {
    partial_private_impls!(Constant);
    fn into_string_buffer(&self, buffer: &mut String) {
        buffer.push_str(self.span());
    }
}

impl EvaluatedNode for Constant {
    partial_node_impls!(Constant);
}

/// A reference to a variable
// #SPC-Variable-Eval.variable_reference
#[derive(Clone, Debug, PartialEq)]
pub struct VariableReference {
    name: EvaluatedTree,
}

impl VariableReference {
    /// Get the evaluated version of the name of this variable
    pub fn name(&self) -> &EvaluatedTree {
        &self.name
    }
}

impl EvaluatedPrivate for VariableReference {
    partial_private_impls!(Variablereference);
}

impl EvaluatedNode for VariableReference {
    partial_node_impls!(VariableReference);
}

/// A reference to a variable that applies some substitution
// #SPC-Variable-Eval.substitution_reference
#[derive(Clone, Debug, PartialEq)]
pub struct SubstitutionReference {
    name: EvaluatedTree,
    pattern: EvaluatedTree,
    replacement: EvaluatedTree,
}

impl SubstitutionReference {
    /// Get the name of the variable that was referenced to construct this node
    pub fn name(&self) -> &EvaluatedTree {
        &self.name
    }

    /// Get the pattern used for replacement
    pub fn pattern(&self) -> &EvaluatedTree {
        &self.pattern
    }

    /// Get the replacement for the pattern
    pub fn replacement(&self) -> &EvaluatedTree {
        &self.replacement
    }
}

impl EvaluatedPrivate for SubstitutionReference {
    partial_private_impls!(SubstitutionReference);
}

impl EvaluatedNode for SubstitutionReference {
    partial_node_impls!(SubstitutionReference);
}

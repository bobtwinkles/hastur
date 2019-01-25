//! This module contains the types used to represent a parsed AST from a
//! variable evaluation context. Evaluating an AST from this module produces an
//! [`Evaluated`](../evaluated/enum.Evaluated.html).

use crate::evaluated::nodes as enodes;
use crate::evaluated::{Block, ContentReference, EvaluatedNode};
use crate::source_location::{LocatedString, Location, Marker};
use crate::VariableName;
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
pub struct AstNode {
    source_location: Marker,
    children: Box<AstChildren>,
}

impl AstNode {
    pub fn location(&self) -> Location {
        self.source_location.inner.clone()
    }

    pub fn children(&self) -> &AstChildren {
        &self.children
    }

    /// Evaluate this AST
    /// Takes a mutable database because Make Is Cursed Technology.
    /// Since `eval` statements are valid anywhere,
    /// AST evaluations *are not* idempotent.
    pub fn eval(&self, context: &mut crate::Database) -> Arc<Block> {
        let (sensitivity, content) = self.eval_internal(context);

        Block::new(sensitivity, content)
    }

    /// Internal evaluation function
    fn eval_internal(
        &self,
        context: &mut crate::Database,
    ) -> (fxhash::FxHashSet<VariableName>, Vec<ContentReference>) {
        let mut sensitivity: fxhash::FxHashSet<VariableName> = Default::default();

        macro_rules! eval_child {
            ($child:expr) => {{
                let child = $child;
                let (new_sens, child_content) = child.eval_internal(context);
                for elem in new_sens.into_iter() {
                    sensitivity.insert(elem);
                }

                child_content
            }};
        }

        macro_rules! eval_subexpr {
            ($e: expr) => {{
                let block = ($e).eval(context);
                for elem in block.sensitivity() {
                    sensitivity.insert(*elem);
                }

                block
            }};
        };

        macro_rules! deref_variable {
            ($name:expr) => {{
                // TODO: rewrite this fragment so we only try interning once.
                // this will require an API for getting variable values from the
                // context using interned variable names.
                let name = $name;
                let value = if let Some(var) = context.get_variable(&name) {
                    let var_ast = var.ast().clone();
                    eval_subexpr!(var_ast)
                } else {
                    Block::empty()
                };
                sensitivity.insert(context.intern_variable_name(name));

                value
            }};
        };

        let content = match self.children() {
            AstChildren::Constant(owned) => vec![ContentReference::new_from_node(Arc::new(
                EvaluatedNode::Constant(owned.clone()),
            ))],
            AstChildren::Concat(ref children) => children
                .iter()
                .map(|child| eval_child!(child))
                .flatten()
                .collect(),
            AstChildren::VariableReference(name) => {
                // Compute the variable name, and then try to evaluate it
                let name = eval_subexpr!(name);
                let value = deref_variable!(name.into_string());

                vec![ContentReference::new_from_node(Arc::new(
                    EvaluatedNode::VariableReference(enodes::VariableReference::new(name, value)),
                ))]
            }
            AstChildren::SubstitutionReference {
                ref variable,
                ref key,
                ref replacement,
            } => {
                let variable = eval_subexpr!(variable);
                let variable_value = deref_variable!(variable.into_string());
                let key = eval_subexpr!(key);
                let replacement = eval_subexpr!(replacement);

                let value = Block::new(
                    sensitivity.clone(),
                    do_subref(variable_value, key.clone(), replacement.clone()),
                );

                vec![ContentReference::new_from_node(Arc::new(
                    EvaluatedNode::SubstitutionReference(enodes::SubstitutionReference::new(
                        variable,
                        key,
                        replacement,
                        value,
                    )),
                ))]
            }
            v => unimplemented!("Node {:?} unimplemented", v),
        };

        (sensitivity, content)
    }
}

/// Parses a variable value, and returns the root of the combined tree node
fn do_subref(
    variable_value: Arc<Block>,
    key: Arc<Block>,
    replacement: Arc<Block>,
) -> Vec<ContentReference> {
    unimplemented!();
}

/// Represents the different types of AST nodes
// #SPC-V-AST
#[derive(Clone, Debug, PartialEq)]
pub enum AstChildren {
    /// An empty node. Has no content at all.
    Empty,
    /// A constant string of text
    /// TODO: This should be a interned reference
    // #SPC-V-AST.constant
    Constant(LocatedString),
    /// Some content that has already been evaluated
    PreEvaluated(Arc<Block>),
    /// Concatenation of several child types
    // #SPC-V-AST.concat
    Concat(Vec<AstNode>),
    /// Reference to a variable
    // #SPC-V-AST.variable_reference
    VariableReference(AstNode),
    /// Reference to a variable, performing substitution
    // #SPC-V-AST.substitution_reference
    SubstitutionReference {
        variable: AstNode,
        key: AstNode,
        replacement: AstNode,
    },
    /// The `strip` make function
    // #SPC-V-AST.strip
    Strip(AstNode),
    /// The `word` make function
    // #SPC-V-AST.word
    Word { index: AstNode, words: AstNode },
    /// The `words` make function
    // #SPC-V-AST.words
    Words(AstNode),
}

#[inline]
pub fn empty() -> AstNode {
    AstNode {
        children: Box::new(AstChildren::Empty),
        source_location: Location::Synthetic.into(),
    }
}

/// Create a new constant node
#[inline]
pub fn constant(source_location: Location, v: LocatedString) -> AstNode {
    AstNode {
        children: Box::new(AstChildren::Constant(v)),
        source_location: source_location.into(),
    }
}

/// Create a new node containing pre-evaluated content
#[inline]
pub fn preevaluated(source_location: Location, v: Arc<Block>) -> AstNode {
    AstNode {
        children: Box::new(AstChildren::PreEvaluated(v)),
        source_location: source_location.into(),
    }
}

/// Create a new concatentation node. This method is slightly smart in that
/// if there is only a single node in the vector, we just return that node.
/// This helps maintain a much more concise tree
#[inline]
pub fn collapsing_concat(source_location: Location, mut v: Vec<AstNode>) -> AstNode {
    if v.len() == 1 {
        v.swap_remove(0)
    } else {
        AstNode {
            children: Box::new(AstChildren::Concat(v)),
            source_location: source_location.into(),
        }
    }
}

/// Create a new variable reference node, using the provided node as the name
#[inline]
pub fn variable_reference(source_location: Location, name: AstNode) -> AstNode {
    AstNode {
        children: Box::new(AstChildren::VariableReference(name)),
        source_location: source_location.into(),
    }
}

/// Create a new `strip` node
pub fn strip(source_location: Location, value: AstNode) -> AstNode {
    AstNode {
        children: Box::new(AstChildren::Strip(value)),
        source_location: source_location.into(),
    }
}

/// Create a new `words` node
pub fn words(source_location: Location, arg: AstNode) -> AstNode {
    AstNode {
        children: Box::new(AstChildren::Words(arg)),
        source_location: source_location.into(),
    }
}

/// Create a new `word` node
pub fn word(source_location: Location, index: AstNode, words: AstNode) -> AstNode {
    AstNode {
        children: Box::new(AstChildren::Word { index, words }),
        source_location: source_location.into(),
    }
}

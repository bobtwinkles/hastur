//! This module contains the types used to represent a parsed AST from a
//! variable evaluation context. Evaluating an AST from this module produces an
//! [`Evaluated`](../evaluated/enum.Evaluated.html).

use crate::evaluated::nodes as enodes;
use crate::evaluated::{Block, ContentReference, EvaluatedNode};
use crate::source_location::{LocatedString, Location, Marker};
use crate::types::Set;
use crate::{Database, NameCache, VariableName};
use std::sync::Arc;

pub mod visit;

mod text_functions;

#[cfg(test)]
mod test;
#[cfg(test)]
pub mod proptest;

/// A node in a syntax tree representing evaluations
#[derive(Clone, Debug, PartialEq)]
pub struct AstNode {
    source_location: Marker,
    children: Box<AstChildren>,
}

impl AstNode {
    /// Get the location of the node
    pub fn location(&self) -> Location {
        self.source_location.inner.clone()
    }

    /// Get the children of this node. This also tells you the type of node
    pub fn children(&self) -> &AstChildren {
        &self.children
    }

    /// Get the children of this node. This also tells you the type of node
    pub fn children_mut(&mut self) -> &mut AstChildren {
        &mut self.children
    }

    /// Evaluate this AST, in the context of the provided database. The provided
    /// database *is not* updated. To fully commit the changes of this
    /// evaluation, one must use `Engine::replace_database` with the returned
    /// database.
    pub fn eval(&self, names: &mut NameCache, context: &Database) -> (Database, Arc<Block>) {
        let (database, sensitivity, content) = self.eval_internal(names, context);

        (database, Block::new(sensitivity, content))
    }

    /// Internal evaluation function
    fn eval_internal(
        &self,
        names: &mut NameCache,
        context: &Database,
    ) -> (Database, Set<VariableName>, Vec<ContentReference>) {
        let mut sensitivity: Set<VariableName> = Default::default();
        // This is technically a little inefficient (we don't always need to do
        // this), but cloning databases should be pretty cheap
        let mut database = context.clone();

        macro_rules! merge_sensitivity {
            ($new_sens:expr) => {
                let new_sens = $new_sens;
                sensitivity = if new_sens.len() < sensitivity.len() {
                    sensitivity.union(new_sens)
                } else {
                    new_sens.union(sensitivity)
                };
            };
        }

        macro_rules! eval_child {
            ($child:expr) => {{
                let child = $child;
                let (new_db, new_sens, child_content) = child.eval_internal(names, context);
                database = new_db;
                merge_sensitivity!(new_sens);

                child_content
            }};
        }

        macro_rules! eval_subexpr {
            ($e: expr) => {{
                let (new_db, block) = ($e).eval(names, context);
                database = new_db;
                merge_sensitivity!(block.raw_sensitivity());

                block
            }};
        };

        macro_rules! deref_variable {
            ($name:expr) => {{
                let interned_variable_name = names.intern_variable_name($name);
                let value = if let Some(var) = context.get_variable(interned_variable_name) {
                    let var_ast = var.ast().clone();
                    eval_subexpr!(var_ast)
                } else {
                    Block::empty()
                };
                sensitivity.insert(interned_variable_name);

                value
            }};
        };

        let content = match self.children() {
            AstChildren::Constant(owned) => vec![ContentReference::new_from_node(Arc::new(
                EvaluatedNode::Constant(owned.clone()),
            ))],
            AstChildren::Concat(ref children) => {
                // Allocate a vector to store the results, guessing that we'll
                // have a 1:1 mapping between children and produced nodes.
                let mut contents = Vec::with_capacity(children.len());
                for child in children {
                    contents.extend(eval_child!(child));
                }
                contents
            }
            AstChildren::PreEvaluated(ref block) => block.content().map(|x| x.clone()).collect(),
            AstChildren::VariableReference(name) => {
                // Compute the variable name, and then try to evaluate it
                let name = eval_subexpr!(name);
                eprintln!("Name is {:?}", name.into_string());
                let colon_find = pair!(
                    name.span(),
                    recognize!(many1!(take_until_and_consume!(":"))),
                    recognize!(many1!(take_until_and_consume!("=")))
                );
                match colon_find {
                    Ok((replacement, (name, key))) => {
                        use nom::Slice;
                        // We need to strip off the terminating : and =, hence the slices
                        let name = name.slice(..name.len() - 1).to_new_block();
                        let key = key.slice(..key.len() - 1).to_new_block();
                        let replacement = replacement.to_new_block();
                        let value = deref_variable!(name.into_string());

                        let value = text_functions::do_subref(
                            sensitivity.clone(),
                            value,
                            key.clone(),
                            replacement.clone(),
                        );

                        vec![ContentReference::new_from_node(Arc::new(
                            EvaluatedNode::SubstitutionReference(
                                enodes::SubstitutionReference::new(name, key, replacement, value),
                            ),
                        ))]
                    }
                    Err(_) => {
                        // If we fail to recognize a replacement, just treat it as a regular reference
                        let value = deref_variable!(name.into_string());

                        vec![ContentReference::new_from_node(Arc::new(
                            EvaluatedNode::VariableReference(enodes::VariableReference::new(
                                name, value,
                            )),
                        ))]
                    }
                }
            }
            AstChildren::Empty => {
                // Empty children generate no content
                Vec::new()
            }
            v => unimplemented!("Node {:?} unimplemented", v),
        };

        (database, sensitivity, content)
    }
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
    /// The `strip` make function
    // #SPC-V-AST.strip
    Strip(AstNode),
    /// The `word` make function
    // #SPC-V-AST.word
    Word {
        /// Determines what index should be used
        index: AstNode,
        /// The list of words to index into
        words: AstNode,
    },
    /// The `words` make function
    // #SPC-V-AST.words
    Words(AstNode),
}

/// Get an empty AST node. These are usually useful as placeholders
#[inline]
pub fn empty() -> AstNode {
    AstNode {
        children: Box::new(AstChildren::Empty),
        source_location: Location::Synthetic.into(),
    }
}

/// Create a new constant node
#[inline]
pub fn constant(v: LocatedString) -> AstNode {
    let location = Marker::from(v.location().clone());
    AstNode {
        children: Box::new(AstChildren::Constant(v)),
        source_location: location,
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
/// This helps maintain a much more concise tree.
/// We also strip out any empties.
#[inline]
pub fn collapsing_concat(source_location: Location, mut v: Vec<AstNode>) -> AstNode {
    // Remove all empties from the concatenation
    v.retain(|node| node.children.as_ref() != &AstChildren::Empty);
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
#[inline]
pub fn strip(source_location: Location, value: AstNode) -> AstNode {
    AstNode {
        children: Box::new(AstChildren::Strip(value)),
        source_location: source_location.into(),
    }
}

/// Create a new `words` node
#[inline]
pub fn words(source_location: Location, arg: AstNode) -> AstNode {
    AstNode {
        children: Box::new(AstChildren::Words(arg)),
        source_location: source_location.into(),
    }
}

/// Create a new `word` node
#[inline]
pub fn word(source_location: Location, index: AstNode, words: AstNode) -> AstNode {
    AstNode {
        children: Box::new(AstChildren::Word { index, words }),
        source_location: source_location.into(),
    }
}

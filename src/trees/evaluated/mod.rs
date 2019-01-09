//! The result of evaluating an AST
use daggy::{Dag, NodeIndex};
use std::any::Any;
use std::rc::Rc;

pub mod nodes;
mod nom;

mod internal {
    use super::*;

    /// Internal methods to seal EvaluatedNode and provide implementation details
    #[doc(hidden)]
    pub trait EvaluatedPrivate {
        fn into_string_buffer(&self, buffer: &mut String) {
            for child in self.children() {
                child.into_string_buffer(buffer);
            }
        }

        fn dynamic_eq(&self, o: &dyn EvaluatedNode) -> bool;
    }
}

// Internal reexport of the private node interface
use self::internal::EvaluatedPrivate;
use self::nodes::*;

/// Represents the evaluated AST.
pub trait EvaluatedNode: std::fmt::Debug + EvaluatedPrivate + Any {
    /// Flatten this node into a string. This function is primarily intended for debugging.
    /// If you want to retain semantic information, use
    /// [`leaves`](trait.EvaluatedNode.html#method.leaves)
    fn into_string(&self) -> String {
        let mut buffer = String::new();
        self.into_string_buffer(&mut buffer);

        buffer
    }

    #[doc(hidden)]
    fn as_private(&self) -> &(dyn EvaluatedPrivate + 'static);

    fn as_any(&self) -> &(Any + 'static);
}

impl PartialEq for EvaluatedNode {
    fn eq(&self, o: &dyn EvaluatedNode) -> bool {
        self.dynamic_eq(o)
    }
}

impl Eq for EvaluatedNode {}

/// A fully evaluated tree
#[derive(Clone, Debug)]
pub struct EvaluatedTree {
    /// The full tree. This implicitly implements #SPC-Variable-Eval.concat
    tree: Dag<Rc<dyn EvaluatedNode>, ()>,
    leaves: Vec<Rc<Constant>>,
    root: NodeIndex,
}

impl PartialEq for EvaluatedTree {
    fn eq(&self, o: &EvaluatedTree) -> bool {
        // TODO: this implementation is probably not correct
        self.leaves == o.leaves
    }
}

impl EvaluatedTree {
    /// Create a new evaluated tree from a DAG of nodes
    fn from_dag(root: daggy::NodeIndex, tree: Dag<Rc<dyn EvaluatedNode>, ()>) -> EvaluatedTree {
        use petgraph::visit::NodeIndexable;
        let mut leaves: Vec<Rc<Constant>> = Vec::new();
        for node_idx in tree.node_bound() {
            let node_idx = tree.from_index(node_idx);
            if tree.children(node_idx).next() == None {
                // There are no children, so this is a leaf
                // Try to convert it into a Rc<Constant>
                if let Some(data) = tree.node_weight(node_idx) {
                    let data: Rc<dyn Any + 'static> = data.clone();
                    let data: &(dyn Any + 'static) = data.clone().as_any();
                    let data: Option<Rc<Constant>> = data.downcast_ref();
                    if let Some(data) = data {
                        leaves.push(data);
                    }
                }
            }
        }

        EvaluatedTree { tree, leaves, root }
    }

    /// Get the flat list of leaves
    pub fn as_leaves(&self) -> &[Rc<Constant>] {
        &self.leaves
    }

    /// Flatten all the leaves into a single string.
    /// This discards all semantic information, so it's mostly useful for
    /// debugging and unit test assertions. If you want to retain semantic
    /// information, consider using
    /// [`as_leaves`](struct.EvaluatedTree.html#method.as_leaves).
    pub fn into_string(&self) -> String {
        let mut buffer = String::new();
        for leaf in self.as_leaves() {
            buffer.push_str(leaf.span());
        }
        buffer
    }

    /// Iterate over the tree in breadth-first order
    pub fn bfs<F>(&self, op: F)
    where
        F: FnMut(&dyn EvaluatedNode) -> (),
    {
        unimplemented!();
    }

    /// Iterate over the tree in depth-first order
    pub fn dfs<F>(&self, op: F)
    where
        F: FnMut(&dyn EvaluatedNode) -> (),
    {
        unimplemented!();
    }

    /// Return this tree to a lightweight reference, suitable for use in parsers
    pub fn as_tree_ref(&self) -> EvaluatedTreeSpan {
        let length = self
            .leaves
            .iter()
            .map(|x| x.span().len())
            .fold(0, |a, b| a + b);
        EvaluatedTreeSpan {
            leaves: &self.leaves,
            offset: 0,
            length: length,
        }
    }
}

/// Represents a span of elements in an evaluated AST
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct EvaluatedTreeSpan<'a> {
    /// The actual contents of this span
    leaves: &'a [Rc<Constant>],
    /// The offset into the first element, in bytes
    offset: usize,
    /// The total length of all string contents
    length: usize,
}

impl<'a> EvaluatedTreeSpan<'a> {
    fn empty() -> EvaluatedTreeSpan<'static> {
        EvaluatedTreeSpan {
            leaves: &[],
            offset: 0,
            length: 0,
        }
    }

    #[inline]
    fn revalidate_offset(&mut self) {
        if self.length == 0 {
            return;
        }

        while self.offset > self.leaves[0].span().len() {
            self.offset -= self.leaves[0].span().len();
            self.leaves = &self.leaves[1..];
        }
    }

    /// Flatten this span into a string.
    pub fn into_string(&self) -> String {
        if self.length == 0 {
            return String::new();
        }

        let mut buffer = String::with_capacity(self.length);
        let mut leaves_iter = self.leaves.iter();
        let mut current = &leaves_iter
            .next()
            .expect("Tree span can not have no leaves and non-0 length")
            .span()
            .as_str()[self.offset..];
        let mut to_push = self.length;
        while to_push > current.len() {
            buffer.push_str(current);
            to_push -= current.len();
            current = leaves_iter
                .next()
                .expect(
                    "Tree span has enough leaves to accumulate the required number of characters",
                )
                .span()
                .as_str();
        }
        buffer.push_str(&current[..to_push]);

        buffer
    }
}

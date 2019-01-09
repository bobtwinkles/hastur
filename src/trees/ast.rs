//! This module contains the types used to represent a parsed AST from a
//! variable evaluation context. Evaluating an AST from this module produces an
//! [`Evaluated`](../evaluated/enum.Evaluated.html).

use crate::source_location::{Location, Marker};
use crate::trees::evaluated::EvaluatedTree;
use crate::trees::evaluated::nodes as enodes;
use crate::Sym;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub struct ASTNode {
    source_location: Marker,
    children: Box<ASTChildren>,
}

impl ASTNode {
    pub fn location(&self) -> Location {
        self.source_location.inner.clone()
    }

    pub fn children(&self) -> &ASTChildren {
        &self.children
    }

    pub fn eval(&self, context: &crate::Database) -> Rc<EvaluatedTree> {
        // #REQ-Expansion-Tracking
        unimplemented!()

        /*
        macro_rules! make_evaluated(
            ($v:expr) => {Evaluated {
                location: self.source_location,
                children: Box::new($v)
            }}
        );

        match self.children() {
            ASTChildren::Constant(sym) => make_evaluated!(EvaluatedChildren::Constant(sym)),
            ASTChildren::Concat(nodes) => make_evaluated!(EvaluatedChildren::Concat(
                nodes.iter().map(|x| x.eval(context)).collect()
            )),
            ASTChildren::VariableReference(variable_name) => {
                let variable_name = variable_name.eval(context);
            }
        }
        */
    }
}

/// Represents the different types of AST nodes
// #SPC-V-AST
#[derive(Clone, Debug, PartialEq)]
pub enum ASTChildren {
    /// A constant string of text
    // #SPC-V-AST.constant
    Constant(Sym),
    /// Concatenation of several child types
    // #SPC-V-AST.concat
    Concat(Vec<ASTNode>),
    /// Reference to a variable
    // #SPC-V-AST.variable_reference
    VariableReference(ASTNode),
    /// Reference to a variable, performing substitution
    // #SPC-V-AST.substitution_reference
    SubstitutionReference {
        variable: ASTNode,
        key: ASTNode,
        replacement: ASTNode,
    },
    /// The `strip` make function
    // #SPC-V-AST.strip
    Strip(ASTNode),
    /// The `word` make function
    // #SPC-V-AST.word
    Word(ASTNode),
    /// The `words` make function
    // #SPC-V-AST.words
    Words(ASTNode),
}

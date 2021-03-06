//! AST walkers
//! TODO: more in-depth documentation

use std::sync::Arc;

use crate::evaluated::Block;
use crate::source_location::LocatedString;

use super::{AstChildren, AstNode};

// This implementation is mostly inspired by the implementation of MIR visitors
// in rustc
macro_rules! make_ast_visitor {
    ($visitor_trait_name:ident, $($mutability:ident)?) => {
        /// See the module documentation for notes on using these, but the
        /// general convention is that you should override the `visit_*`
        /// methods, calling the corresponding `super_*` method when you wish to
        /// recurse.
        pub trait $visitor_trait_name <'node> {
            /// Recurse into an AST node
            fn visit_ast(&mut self, ast: &'node $($mutability)? AstNode) {
                self.super_ast(ast);
            }

            /// Visit an `Empty` node
            fn visit_empty(&mut self) {
                self.super_empty();
            }

            /// Visit a `Constant` node
            fn visit_constant(&mut self, content: &'node $($mutability)? LocatedString) {
                self.super_constant(content);
            }

            /// Visit a `PreEvaluated` node
            fn visit_pre_evaluated(&mut self, block: &'node $($mutability)? Arc<Block>) {
                self.super_pre_evaluated(block);
            }

            /// Visit a `Concat` node
            fn visit_concat(&mut self, nodes: &'node $($mutability)? Vec<AstNode>) {
                self.super_concat(nodes);
            }

            /// Visit a `VariableReference` node
            fn visit_variable_reference(&mut self, name: &'node $($mutability)? AstNode) {
                self.super_variable_reference(name);
            }

            /// Visit a `Eval` node
            fn visit_eval(&mut self, content: &'node $($mutability)? AstNode) {
                self.super_eval(content);
            }

            /// Visit a `Strip` node
            fn visit_strip(&mut self, content: &'node $($mutability)? AstNode) {
                self.super_strip(content);
            }

            /// Visit a `Word` node
            fn visit_word(&mut self, index: &'node $($mutability)? AstNode, words: &'node $($mutability)? AstNode) {
                self.super_word(index, words);
            }

            /// Visit a `Words` node
            fn visit_words(&mut self, words: &'node $($mutability)? AstNode) {
                self.super_words(words);
            }

            /// Recursion implementation for `Empty` nodes
            fn super_empty(&mut self) {}

            /// Recursion implementation for `Constant` nodes
            fn super_constant(&mut self, _content: &'node $($mutability)? LocatedString) {}

            /// Recursion implementation for `PreEvaluated` nodes
            fn super_pre_evaluated(&mut self, _block: &'node $($mutability)? Arc<Block>) {}

            /// Recursion implementation for `Concat` nodes
            fn super_concat(&mut self, nodes: &'node $($mutability)? Vec<AstNode>) {
                macro_rules! children {
                    (mut) => (nodes.iter_mut());
                    () => (nodes.iter());
                }
                for node in children!($($mutability)?) {
                    self.visit_ast(node);
                }
            }

            /// Recursion implementation for `VariableReference` nodes
            fn super_variable_reference(&mut self, name: &'node $($mutability)? AstNode) {
                self.visit_ast(name);
            }

            /// Super a `Eval` node
            fn super_eval(&mut self, content: &'node $($mutability)? AstNode) {
                self.visit_ast(content);
            }

            /// Recursion implementation for `Strip` nodes
            fn super_strip(&mut self, content: &'node $($mutability)? AstNode) {
                self.visit_ast(content);
            }

            /// Recursion implementation for `Word` nodes
            fn super_word(&mut self, index: &'node $($mutability)? AstNode, words: &'node $($mutability)? AstNode) {
                self.visit_ast(index);
                self.visit_ast(words);
            }

            /// Recursion implementation for `Words` nodes
            fn super_words(&mut self, content: &'node $($mutability)? AstNode) {
                self.visit_ast(content);
            }

            /// Core recursion on AST nodes
            fn super_ast(&mut self, ast: &'node $($mutability)? AstNode) {
                macro_rules! children {
                    (mut) => (ast.children_mut());
                    () => (ast.children());
                }
                match children!($($mutability)?) {
                    AstChildren::Empty => {
                        self.visit_empty()
                    }
                    AstChildren::Constant(s) => {
                        self.visit_constant(s)
                    }
                    AstChildren::PreEvaluated(block) => {
                        self.visit_pre_evaluated(block);
                    }
                    AstChildren::Concat(nodes) => {
                        self.visit_concat(nodes);
                    }
                    AstChildren::VariableReference(node) => {
                        self.visit_variable_reference(node);
                    }
                    AstChildren::Eval(child) => {
                        self.visit_eval(child);
                    }
                    AstChildren::Strip(child) => {
                        self.visit_strip(child);
                    }
                    AstChildren::Word { index, words } => {
                        self.visit_word(index, words);
                    }
                    AstChildren::Words(child) => {
                        self.visit_words(child);
                    }
                }
            }
        }
    }
}

make_ast_visitor!(Visitor,);
make_ast_visitor!(MutVisitor, mut);

//! Various proptest generators

use crate::ast::AstChildren;
use crate::ast::AstNode;
use crate::source_location::{LocatedString, Location};
use proptest::prelude::*;

pub fn alphanum_string() -> impl Strategy<Value = String> {
    "[a-zA-Z0-9]+"
}

pub fn arb_ast(tree_breadth: u32) -> impl Strategy<Value = AstNode> {
    let leaf = prop_oneof!(
        alphanum_string().prop_map(|s| AstChildren::Constant(LocatedString::test_new(0, 0, s)))
    );
    let child = leaf.prop_recursive(3, 32, tree_breadth, move |inner| {
        // Transform the inner strategy to one on AST nodes
        let inner = inner.prop_map(|x| AstNode {
            source_location: Location::test_location(0, 0).into(),
            children: Box::new(x),
        });
        prop_oneof![
            // prop::collection::vec(inner.clone(), 2..(tree_breadth as usize))
            //     .prop_map(AstChildren::Concat),
            inner.clone().prop_map(AstChildren::VariableReference),
            inner.clone().prop_map(AstChildren::Eval),
            inner.clone().prop_map(AstChildren::Strip),
            (inner.clone(), inner.clone())
                .prop_map(|(a, b)| AstChildren::Word { index: a, words: b }),
            inner.clone().prop_map(AstChildren::Words),
        ]
    });
    child.prop_map(|child| {
        // Fix up all the locations
        AstNode {
            source_location: Location::test_location(1, 1).into(),
            children: Box::new(child),
        }
    })
}

pub fn arb_flat_ast(ast_breadth: u32) -> impl Strategy<Value = (AstNode, String)> {
    use crate::evaluated::Block;
    use std::sync::Arc;

    struct FlattenWalker<'a> {
        current_char: usize,
        strings: Vec<&'a str>,
    }

    impl<'a> FlattenWalker<'a> {
        fn visit_function_pre(&mut self, fname: &'a str) {
            self.strings.push("$(");
            self.strings.push(fname);
            self.strings.push(" ");
            self.current_char += 3 + fname.chars().count();
        }

        fn visit_function_post(&mut self) {
            self.strings.push(")");
            self.current_char += 1;
        }

        fn push_string(&mut self, s: &'a str) {
            self.strings.push(s);
            self.current_char += s.chars().count();
        }
    }

    impl<'a> crate::ast::visit::MutVisitor<'a> for FlattenWalker<'a> {
        fn visit_ast(&mut self, node: &'a mut AstNode) {
            node.source_location = Location::test_location(1, self.current_char as u32).into();

            self.super_ast(node);
        }

        fn visit_constant(&mut self, content: &'a mut LocatedString) {
            content.location = Location::test_location(1, self.current_char as u32).into();
            self.strings.push(&content.contents);
            self.current_char += content.chars().count();
        }

        fn visit_pre_evaluated(&mut self, block: &'a mut Arc<Block>) {
            self.super_pre_evaluated(block);
            unimplemented!("Flattening of pre-evaluated blocks");
        }

        fn visit_concat(&mut self, nodes: &'a mut Vec<AstNode>) {
            self.super_concat(nodes);
        }

        fn visit_variable_reference(&mut self, name: &'a mut AstNode) {
            self.push_string("$(");

            self.super_variable_reference(name);

            self.push_string(")");
        }

        fn visit_abspath(&mut self, content: &'a mut AstNode) {
            self.visit_function_pre("abspath");
            self.super_abspath(content);
            self.visit_function_post();
        }

        fn visit_eval(&mut self, content: &'a mut AstNode) {
            self.visit_function_pre("eval");
            self.super_eval(content);
            self.visit_function_post();
        }

        fn visit_firstword(&mut self, content: &'a mut AstNode) {
            self.visit_function_pre("firstword");
            self.super_firstword(content);
            self.visit_function_post();
        }

        fn visit_findstring(&mut self, needle: &'a mut AstNode, haystack: &'a mut AstNode) {
            self.visit_function_pre("findstring");
            self.super_findstring(needle, haystack);
            self.visit_function_post();
        }

        fn visit_if(
            &mut self,
            condition: &'a mut AstNode,
            true_case: &'a mut AstNode,
            false_case: &'a mut AstNode,
        ) {
            self.visit_function_pre("if");
            self.super_if(condition, true_case, false_case);
            self.visit_function_post();
        }

        fn visit_strip(&mut self, content: &'a mut AstNode) {
            self.visit_function_pre("strip");
            self.super_strip(content);
            self.visit_function_post();
        }

        fn visit_shell(&mut self, content: &'a mut AstNode) {
            self.visit_function_pre("shell");
            self.super_shell(content);
            self.visit_function_post();
        }

        fn visit_word(&mut self, index: &'a mut AstNode, words: &'a mut AstNode) {
            self.visit_function_pre("word");
            // XXX: we should technically be not doing this, but it's cowboy time
            self.visit_ast(index);
            self.push_string(",");
            self.visit_ast(words);

            self.visit_function_post();
        }

        fn visit_words(&mut self, words: &'a mut AstNode) {
            self.visit_function_pre("words");
            self.super_words(words);
            self.visit_function_post();
        }
    }

    let ast = arb_ast(ast_breadth);

    ast.prop_map(|mut ast| {
        use crate::ast::visit::MutVisitor;

        let mut walker = FlattenWalker {
            current_char: 1,
            strings: Vec::new(),
        };
        walker.visit_ast(&mut ast);

        let mut flat = String::with_capacity(walker.current_char);
        for s in walker.strings.into_iter() {
            flat.push_str(s);
        }

        (ast, flat)
    })
}

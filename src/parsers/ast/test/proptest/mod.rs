//! Property-based testing of the AST parser
//! Includes the property tests themselves, as well as explicit tests where bugs
//! where discovered

use crate::ast::proptest::arb_flat_ast;
use crate::parsers::ast::parse_ast;
use crate::parsers::test::create_span;
use proptest::prelude::*;

proptest!(
    #[test]
    fn ast_parse_roundtrip((ast, flat) in arb_flat_ast(4)) {
        let block = create_span(&flat);
        let span = block.span();
        match parse_ast(span) {
            Ok((_, ast_prime)) => {
                prop_assert_eq!(ast_prime, ast);
            }
            Err(e) => {
                prop_assert!(false, "Parsing failed {:?}", e)
            }
        }
    }
);

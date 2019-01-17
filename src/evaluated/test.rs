use super::*;
use crate::source_location::{Located, LocatedString, Location};

fn test_span(content: &str) -> LocatedString {
    Located::new(
        Location::TestLocation {
            line: 1,
            character: 1,
        }
        .into(),
        content.into(),
    )
}

fn test_constant_node(content: &str) -> Arc<EvaluatedNode> {
    Arc::new(EvaluatedNode::Constant(test_span(content)))
}

#[test]
fn simple_block_iter() {
    let block = Block::new(
        Default::default(),
        vec![ContentReference::new_from_node(test_constant_node("asdf"))],
    );

    let result: String = block.span().chars().collect();
    assert!(result == "asdf")
}

#[test]
fn content_offset() {
    let block = Block::new(
        Default::default(),
        vec![ContentReference::new_from_node_slice(
            test_constant_node("asdf"),
            2,
            2,
        )],
    );

    let result: String = block.span().chars().collect();
    assert!(result == "df")
}

#[test]
fn slice_compound_offset() {
    let block = Block::new(
        Default::default(),
        vec![
            ContentReference::new_from_node(test_constant_node("asdf")),
            ContentReference::new_from_node(test_constant_node("fdsa")),
        ],
    );

    let mut span = block.span();
    span.offset = 2;
    span.length -= 4;

    let result: String = span.chars().collect();
    assert!(result == "dffd");
}

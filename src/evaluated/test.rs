use super::*;
use crate::source_location::test::{span, span_with_location};
use crate::source_location::{LocatedString, Location};

pub(crate) fn concat_node_with_locations(content: &[(&str, Location)]) -> Arc<Block> {
    let nodes = content
        .iter()
        .map(|(content, location)| {
            ContentReference::new_from_node(Arc::new(EvaluatedNode::Constant(LocatedString::new(
                location.clone().into(),
                (*content).into(),
            ))))
        })
        .collect();
    Block::new(
        Default::default(),
        vec![ContentReference::new_from_node(Arc::new(
            EvaluatedNode::Concat(Block::new(Default::default(), nodes)),
        ))],
    )
}

pub(crate) fn single_block(content: &str) -> Arc<Block> {
    concat_node_with_locations(&[(content, Location::test_location(1, 1))])
}

pub(crate) fn test_constant_node(content: &str) -> Arc<EvaluatedNode> {
    Arc::new(EvaluatedNode::Constant(span(content)))
}

pub(crate) fn test_concat_node<'a>(content: impl AsRef<[&'a str]>) -> Arc<EvaluatedNode> {
    let content = content.as_ref();
    Arc::new(EvaluatedNode::Concat(Block::new(
        Default::default(),
        content
            .iter()
            .map(|x| ContentReference::new_from_node(test_constant_node(x)))
            .collect(),
    )))
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
            ContentReference::new_from_node(test_constant_node("abcd")),
            ContentReference::new_from_node(test_constant_node("efgh")),
        ],
    );

    let mut span = block.span();
    span.offset = 2;
    span.length -= 4;

    let result: String = span.chars().collect();
    assert!(result == "cdef");
}

#[test]
fn simplify_drop_end() {
    let mut block = Block::new(
        Default::default(),
        vec![ContentReference::new_from_node_slice(
            test_concat_node(["abcd", "efgh"]),
            0,
            4,
        )],
    );
    Arc::make_mut(&mut block).simplify();

    assert_eq!(
        &block.content,
        &[ContentReference::new_from_node(test_concat_node(["abcd"]))]
    );
}

#[test]
fn simplify_shorten_end() {
    let mut block = Block::new(
        Default::default(),
        vec![ContentReference::new_from_node_slice(
            test_concat_node(["abcd", "efgh"]),
            0,
            6,
        )],
    );
    Arc::make_mut(&mut block).simplify();

    assert_eq!(
        &block.content,
        &[ContentReference::new_from_node(test_concat_node([
            "abcd", "ef"
        ]))]
    );
}

#[test]
fn simplify_shorten_start() {
    let mut block = Block::new(
        Default::default(),
        vec![ContentReference::new_from_node_slice(
            test_concat_node(["abcd", "efgh"]),
            2,
            2,
        )],
    );
    Arc::make_mut(&mut block).simplify();

    assert_eq!(
        &block.content,
        &[ContentReference::new_from_node(Arc::new(
            EvaluatedNode::Concat(Block::new(
                Default::default(),
                vec![ContentReference::new_from_node(Arc::new(
                    EvaluatedNode::Constant(span_with_location("cd", 1, 3))
                ))]
            ))
        ))]
    );
}

#[test]
fn simplify_drop_start() {
    let mut block = Block::new(
        Default::default(),
        vec![ContentReference::new_from_node_slice(
            test_concat_node(["abcd", "efgh"]),
            4,
            4,
        )],
    );
    Arc::make_mut(&mut block).simplify();

    assert_eq!(
        &block.content,
        &[ContentReference::new_from_node(test_concat_node(["efgh"]))]
    );
}

#[test]
fn iterate_indices() {
    let block = concat_node_with_locations(&[("a你好", Location::test_location(1, 1))]);
    let span = block.span();

    let mut iter = span.char_indices();

    assert_eq!(iter.next(), Some((0, 'a')));
    assert_eq!(iter.next(), Some((1, '你')));
    assert_eq!(iter.next(), Some((4, '好')));
    assert_eq!(iter.next(), None);
}

#[test]
fn iterate_indices_simplified() {
    let block = Block::new(
        Default::default(),
        vec![ContentReference::new_from_node(test_constant_node(
            "\"a\" \"b\"",
        ))],
    );
    let span = block.span();

    let mut iter = span.char_indices();

    assert_eq!(iter.next(), Some((0, '"')));
    assert_eq!(iter.next(), Some((1, 'a')));
    assert_eq!(iter.next(), Some((2, '"')));
    assert_eq!(iter.next(), Some((3, ' ')));
    assert_eq!(iter.next(), Some((4, '"')));
    assert_eq!(iter.next(), Some((5, 'b')));
    assert_eq!(iter.next(), Some((6, '"')));
    assert_eq!(iter.next(), None);
}

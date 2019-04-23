//! The lexer for makefile lines

use super::tokenizer::{self, Token, TokenType};

mod lexer_grammar;

/// A parsed line of a makefile
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MakefileLine {
    /// A line that starts with a conditional directive
    ConditionalLine(ConditionalLine),
    /// An empty line
    EmptyLine,
}

/// This line is a conditional
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ConditionalLine {
    start: usize,
    conditional: ConditionalTy,
    end: usize,
}

/// What type of conditional line is it?
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ConditionalTy {
    /// An `ifeq` statement
    IfEq(VariableAstNode, VariableAstNode),
    /// An `ifneq` statement
    IfNEq(VariableAstNode, VariableAstNode),
    /// An `ifdef` statement
    IfDef(VariableAstNode),
    /// An `ifndef` statement
    IfNDef(VariableAstNode),
    /// An `else` statement, with an optional follow-on conditional
    Else(Option<Box<ConditionalTy>>),
    /// An `endif` statement
    Endif,
}

/// A variable AST node capture.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VariableAstNode {
    /// The start location of the content
    start: usize,
    /// Child node, including the type
    ty: VariableAstNodeTy,
    /// The last index (exclusive of the content)
    end: usize,
}

impl VariableAstNode {
    #[cfg(test)]
    fn new(start: usize, ty: VariableAstNodeTy, end: usize) -> Self {
        Self { start, ty, end }
    }
}

/// What kind of AST node is it?
#[derive(Clone, Debug, PartialEq, Eq)]
#[allow(dead_code)]
pub enum VariableAstNodeTy {
    /// Constant text
    Text,

    /// Concatenation of other nodes,
    Concat(Vec<VariableAstNode>),

    /// A reference to a variable.
    VariableReference {
        /// The name of the variable being referenced
        name: Box<VariableAstNode>,
    },

    /// The `abspath` function
    Abspath(Box<VariableAstNode>),
    /// The `addprefix` function
    AddPrefix(NotYetImplemented),
    /// The `addprefix` function
    AddSuffix(NotYetImplemented),
    /// The `and` function
    And(NotYetImplemented),
    /// The `basename` function
    BaseName(NotYetImplemented),
    /// The `call` function
    Call(NotYetImplemented),
    /// The `dir` function
    Dir(NotYetImplemented),
    /// The `error` function
    Error(NotYetImplemented),
    /// The `eval` function
    Eval(NotYetImplemented),
    /// The `file` function
    File(NotYetImplemented),
    /// The `filter` function
    Filter(NotYetImplemented),
    /// The `filterout` function
    FilterOut(NotYetImplemented),
    /// The `findstring` function
    FindString(NotYetImplemented),
    /// The `firstword` function
    FirstWord(NotYetImplemented),
    /// The `flavor` function
    Flavor(NotYetImplemented),
    /// The `if` function
    If(NotYetImplemented),
    /// The `info` function
    Info(NotYetImplemented),
    /// The `join` function
    Join(NotYetImplemented),
    /// The `lastword` function
    LastWord(NotYetImplemented),
    /// The `notdir` function
    NotDir(NotYetImplemented),
    /// The `or` function
    Or(NotYetImplemented),
    /// The `origin` function
    Origin(NotYetImplemented),
    /// The `patsubst` function
    PatSubst(NotYetImplemented),
    /// The `realpath` function
    Realpath(NotYetImplemented),
    /// The `sort` function
    Sort(NotYetImplemented),
    /// The `strip` function
    Strip(NotYetImplemented),
    /// The `subst` function
    Subst(NotYetImplemented),
    /// The `suffix` function
    Suffix(NotYetImplemented),
    /// The `value` function
    Value(NotYetImplemented),
    /// The `warning` function
    Warning(NotYetImplemented),
    /// The `wildcard` function
    Wildcard(NotYetImplemented),
    /// The `word` function
    Word(NotYetImplemented),
    /// The `wordlist` function
    WordList(NotYetImplemented),
    /// The `words` function
    Words(NotYetImplemented),
}

/// Indicates that parsing (and everything downstream) for this function is not implemented yet
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NotYetImplemented {}

/// There are no lexical errors when parsing Makefiles
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum LexicalError {}

/// Convert an arbitrary token into a text node
fn token_as_text(start: usize, _tok: TokenType, end: usize) -> VariableAstNode {
    VariableAstNode {
        start,
        end,
        ty: VariableAstNodeTy::Text,
    }
}

/// Adapts an iterator over tokens to an iterator over spans
fn adapt_token_iterator(
    it: impl Iterator<Item = Token>,
) -> impl Iterator<Item = (usize, TokenType, usize)> {
    it.map(|token| (token.start, token.token_type, token.end))
}

/// Parse a stream of tokens into a line
/// TODO: this shouldn't return the lalrpop error directly, as that exposes lalrpop_util as a public dep
pub fn parse_stream(
    it: impl Iterator<Item = Token>,
) -> Result<MakefileLine, lalrpop_util::ParseError<usize, tokenizer::TokenType, LexicalError>> {
    let parser = lexer_grammar::MakefileLineParser::new();

    parser.parse(adapt_token_iterator(it))
}

#[cfg(test)]
mod test {
    use super::adapt_token_iterator;
    use super::lexer_grammar::MakefileLineParser;
    use super::{ConditionalLine, ConditionalTy, MakefileLine, VariableAstNode, VariableAstNodeTy};
    use crate::tokenizer::iterator_to_token_stream;
    use crate::tokenizer::TokenType;
    use pretty_assertions::assert_eq;

    fn simple_iterator(s: &'static str) -> impl Iterator<Item = (usize, TokenType, usize)> {
        adapt_token_iterator(iterator_to_token_stream(s.char_indices()))
    }

    mod whitespace {
        use super::*;
        use pretty_assertions::assert_eq;

        #[test]
        fn simple() {
            let res: MakefileLine =
                assert_ok!(MakefileLineParser::new().parse(simple_iterator("  \t")));
            assert_eq!(MakefileLine::EmptyLine, res);
        }
    }

    mod ifeq {
        use super::*;
        use pretty_assertions::assert_eq;

        #[test]
        fn simple() {
            let res: MakefileLine =
                assert_ok!(MakefileLineParser::new().parse(simple_iterator("ifeq(a, b)")));
            assert_eq!(
                MakefileLine::ConditionalLine(ConditionalLine {
                    start: 0,
                    conditional: ConditionalTy::IfEq(
                        VariableAstNode::new(5, VariableAstNodeTy::Text, 6),
                        VariableAstNode::new(8, VariableAstNodeTy::Text, 9)
                    ),
                    end: 10,
                }),
                res
            )
        }

        #[test]
        fn quote() {
            let res: MakefileLine =
                assert_ok!(MakefileLineParser::new().parse(simple_iterator("ifeq \"a\" 'b'")));
            assert_eq!(
                MakefileLine::ConditionalLine(ConditionalLine {
                    start: 0,
                    conditional: ConditionalTy::IfEq(
                        VariableAstNode::new(6, VariableAstNodeTy::Text, 7),
                        VariableAstNode::new(10, VariableAstNodeTy::Text, 11)
                    ),
                    end: 12,
                }),
                res
            )
        }

        #[test]
        fn followed_by_comment() {
            crate::test::setup();
            const input: &'static str = "ifeq(a, b) # this is a comment";
            let res: MakefileLine =
                assert_ok!(MakefileLineParser::new().parse(simple_iterator(input)));
            assert_eq!(
                MakefileLine::ConditionalLine(ConditionalLine {
                    start: 0,
                    conditional: ConditionalTy::IfEq(
                        VariableAstNode::new(5, VariableAstNodeTy::Text, 6),
                        VariableAstNode::new(8, VariableAstNodeTy::Text, 9),
                    ),
                    /// XXX: This is a bit annoying, but without significantly
                    /// refactoring the grammar it's hard to avoid capturing the
                    /// tailing whitespace here
                    end: 11,
                }),
                res
            )
        }
    }

    mod ifdef {
        use super::*;
        use pretty_assertions::assert_eq;

        #[test]
        fn simple() {
            crate::test::setup();
            const input: &'static str = "ifdef foo";
            let res: MakefileLine =
                assert_ok!(MakefileLineParser::new().parse(simple_iterator(input)));
            assert_eq!(
                MakefileLine::ConditionalLine(ConditionalLine {
                    start: 0,
                    conditional: ConditionalTy::IfDef(VariableAstNode::new(
                        6,
                        VariableAstNodeTy::Text,
                        9
                    ),),
                    end: 9,
                }),
                res
            )
        }
    }
}

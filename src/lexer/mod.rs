use super::tokenizer::{self, IsDoubleColon, Token, TokenType, VariableAssign};

mod lexer_grammar;

/// A parsed line of a makefile
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MakefileLine {
    ConditionalLine(ConditionalLine),
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
    IfEq(VariableAstNode, VariableAstNode),
    IfNEq(VariableAstNode, VariableAstNode),
    IfDef(VariableAstNode),
    IfNDef(VariableAstNode),
    Else(Option<Box<ConditionalTy>>),
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

/// What kind of AST node is it?
#[derive(Clone, Debug, PartialEq, Eq)]
#[allow(dead_code)]
pub enum VariableAstNodeTy {
    /// Constant text
    Text,

    /// Concatenation of other nodes,
    Concat(Vec<VariableAstNode>),

    /// A reference to a variable.
    /// The various location fields relate as follows:
    /// ```
    /// $(words a list of words)
    /// ^                     ^
    /// |                     \_ end
    /// \_ start
    /// ```
    VariableReference {
        name: Box<VariableAstNode>,
    },

    /// The `abspath` function
    Abspath(Box<VariableAstNode>),
    AddPrefix(NotYetImplemented),
    AddSuffix(NotYetImplemented),
    And(NotYetImplemented),
    BaseName(NotYetImplemented),
    Call(NotYetImplemented),
    Dir(NotYetImplemented),
    Error(NotYetImplemented),
    Eval(NotYetImplemented),
    File(NotYetImplemented),
    Filter(NotYetImplemented),
    FilterOut(NotYetImplemented),
    FindString(NotYetImplemented),
    FirstWord(NotYetImplemented),
    Flavor(NotYetImplemented),
    If(NotYetImplemented),
    Info(NotYetImplemented),
    Join(NotYetImplemented),
    LastWord(NotYetImplemented),
    NotDir(NotYetImplemented),
    Or(NotYetImplemented),
    Origin(NotYetImplemented),
    PatSubst(NotYetImplemented),
    Realpath(NotYetImplemented),
    Sort(NotYetImplemented),
    Strip(NotYetImplemented),
    Subst(NotYetImplemented),
    Suffix(NotYetImplemented),
    Value(NotYetImplemented),
    Warning(NotYetImplemented),
    Wildcard(NotYetImplemented),
    Word(NotYetImplemented),
    WordList(NotYetImplemented),
    Words(NotYetImplemented),
}

/// Indicates that parsing (and everything downstream) for this function is not implemented yet
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NotYetImplemented {}

fn as_text(start: usize, end: usize) -> VariableAstNode {
    VariableAstNode {
        start,
        end,
        ty: VariableAstNodeTy::Text,
    }
}

/// There are no lexical errors when parsing Makefiles
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum LexicalError {}

/// Convert an arbitrary token into a text node
fn token_as_text(start: usize, tok: TokenType, end: usize) -> VariableAstNode {
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

    #[test]
    fn simple_whitespace() {
        let res: MakefileLine =
            assert_ok!(MakefileLineParser::new().parse(simple_iterator("  \t")));
        assert_eq!(MakefileLine::EmptyLine, res);
    }

    #[test]
    fn simple_ifeq() {
        let res: MakefileLine =
            assert_ok!(MakefileLineParser::new().parse(simple_iterator("ifeq(a, b)")));
        assert_eq!(
            MakefileLine::ConditionalLine(ConditionalLine {
                start: 0,
                conditional: ConditionalTy::IfEq(
                    VariableAstNode {
                        // The "a"
                        start: 5,
                        end: 6,
                        ty: VariableAstNodeTy::Text,
                    },
                    VariableAstNode {
                        start: 7,
                        end: 9,
                        ty: VariableAstNodeTy::Concat(vec![
                            VariableAstNode {
                                // The whitespace
                                start: 7,
                                ty: VariableAstNodeTy::Text,
                                end: 8,
                            },
                            VariableAstNode {
                                // The "b"
                                start: 8,
                                ty: VariableAstNodeTy::Text,
                                end: 9,
                            }
                        ]),
                    }
                ),
                end: 10,
            }),
            res
        )
    }
}

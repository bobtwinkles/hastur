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
    EndIf,
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
    use lalrpop_util::ParseError;

    macro_rules! run_parser_init {
        ($i:expr) => {{
            crate::test::setup();
            run_parser!($i)
        }};
    }

    macro_rules! run_parser {
        ($i:expr) => {{
            MakefileLineParser::new().parse(simple_iterator($i))
        }};
    }

    macro_rules! assert_unrecognized_token {
        ($err:expr) => {{
            let err = $err;
            if let ParseError::UnrecognizedToken {
                token,
                expected: _expected,
            } = err
            {
                token
            } else {
                panic!("Unexpected error {:?}", err)
            }
        }};
    }

    fn simple_iterator(s: &'static str) -> impl Iterator<Item = (usize, TokenType, usize)> {
        adapt_token_iterator(iterator_to_token_stream(s.char_indices()))
    }

    mod whitespace {
        use super::*;
        use pretty_assertions::assert_eq;

        #[test]
        fn simple() {
            let res = assert_ok!(run_parser_init!("  \t"));
            assert_eq!(MakefileLine::EmptyLine, res);
        }
    }

    mod ifeq {
        use super::*;
        use pretty_assertions::assert_eq;

        #[test]
        fn simple() {
            let res = assert_ok!(run_parser_init!("ifeq(a, b)"));
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
            let res = assert_ok!(run_parser_init!("ifeq \"a\" 'b'"));
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
            let res = assert_ok!(run_parser_init!("ifeq(a, b) # this is a comment"));
            assert_eq!(
                MakefileLine::ConditionalLine(ConditionalLine {
                    start: 0,
                    conditional: ConditionalTy::IfEq(
                        VariableAstNode::new(5, VariableAstNodeTy::Text, 6),
                        VariableAstNode::new(8, VariableAstNodeTy::Text, 9),
                    ),
                    end: 10,
                }),
                res
            )
        }

        #[test]
        fn unterminated_parens() {
            let res = assert_err!(run_parser_init!("ifeq(a, b"));
            let token = assert_unrecognized_token!(res);
            assert!(token.is_none());

            let res = assert_err!(run_parser!("ifeq(a"));
            let token = assert_unrecognized_token!(res);
            assert!(token.is_none());
        }
    }

    mod ifdef {
        use super::*;
        use pretty_assertions::assert_eq;

        #[test]
        fn simple() {
            let res = assert_ok!(run_parser_init!("ifdef foo"));
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

        #[test]
        fn no_content() {
            let res = assert_err!(run_parser_init!("ifdef "));
            let token = assert_unrecognized_token!(res);
            assert!(token.is_none());

            let res = assert_err!(run_parser!("ifdef # comment after ifdef"));
            let token = assert_unrecognized_token!(res);
            assert_eq!(token, Some((6, TokenType::CommentStart, 7)))
        }
    }

    mod _else {
        use super::*;
        use pretty_assertions::assert_eq;

        #[test]
        fn simple() {
            let res = assert_ok!(run_parser_init!("else"));
            assert_eq!(
                MakefileLine::ConditionalLine(ConditionalLine {
                    start: 0,
                    conditional: ConditionalTy::Else(None),
                    end: 4,
                }),
                res
            );
        }

        #[test]
        fn followup_cond() {
            let res = assert_ok!(run_parser_init!("else ifdef foo"));
            assert_eq!(
                MakefileLine::ConditionalLine(ConditionalLine {
                    start: 0,
                    conditional: ConditionalTy::Else(Some(Box::new(ConditionalTy::IfDef(
                        VariableAstNode::new(11, VariableAstNodeTy::Text, 14)
                    )))),
                    end: 14
                }),
                res
            );

            let res = assert_ok!(run_parser!("else ifeq 'foo' 'bar'"));
            assert_eq!(
                MakefileLine::ConditionalLine(ConditionalLine {
                    start: 0,
                    conditional: ConditionalTy::Else(Some(Box::new(ConditionalTy::IfEq(
                        VariableAstNode::new(11, VariableAstNodeTy::Text, 14),
                        VariableAstNode::new(17, VariableAstNodeTy::Text, 20)
                    )))),
                    end: 21
                }),
                res
            )
        }

        #[test]
        fn followup_invalid() {
            let res = assert_err!(run_parser_init!("else foo"));
            let tok = assert_unrecognized_token!(res);
            assert_eq!(tok, Some((5, TokenType::Text, 8)));
        }
    }

    mod endif {
        use super::*;
        use pretty_assertions::assert_eq;

        #[test]
        fn simple() {
            let res = assert_ok!(run_parser_init!("endif"));
            assert_eq!(
                MakefileLine::ConditionalLine(ConditionalLine {
                    start: 0,
                    conditional: ConditionalTy::EndIf,
                    end: 5
                }),
                res
            );
        }

        #[test]
        fn extra_tokens() {
            let res = assert_err!(run_parser_init!("endif foo"));
            let tok = assert_unrecognized_token!(res);
            assert_eq!(tok, Some((6, TokenType::Text, 9)));
        }
    }
}

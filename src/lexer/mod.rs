use super::tokenizer::{self, IsDoubleColon, VariableAssign};

// mod lexer_grammar;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Lexeme {
    /// Not at all special text
    NormalText,
    /// Some whitespace
    Whitespace,
    /// An escaped character
    EscapedCharacter(Option<char>),
    /// A variable assignment operator
    VariableAssign(VariableAssign),
    /// A colon
    Colonish(IsDoubleColon),
    /// An open parenthesis
    OpenParen,
    /// A close parenthesis
    CloseParen,
    /// An open brace `{`
    OpenBrace,
    /// A close brace `}`
    CloseBrace,
    /// A colon (or double colon)
    Colon(IsDoubleColon),
    /// A semicolon `;`
    SemiColon,
    /// A percent `%`
    Percent,
    /// An unescaped newline
    NewLine,
    /// A `#` character
    CommentStart,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum MakefileLine {
    ConditionalLine(ConditionalLine),
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct ConditionalLine {
    start: usize,
    // conditional: ConditionalTy
    end: usize,
}

/// A variable AST node capture.
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
struct VariableAstNode {
    /// The start location of the content
    start: usize,
    /// Child node, including the type
    ty: VariableAstNodeTy,
    /// The last index (exclusive of the content)
    end: usize,
}

#[derive(Clone, Debug, PartialEq)]
enum VariableAstNodeTy {
    /// Constant text
    Text,

    /// Concatenation of other nodes,
    Concat(Vec<VariableAstNode>),

    /// A reference to a variable.
    /// The various location fields relate as follows:
    /// ```
    /// $(words a list of words)
    /// ^ ^                   ^
    /// | \_ start            \_ end
    /// \_ dollar
    VariableReference {
        dollar: usize,
    }

    /// The `abspath` function
    Abspath(Box<VariableAstNode>),
    AddPrefix(!),
    AddSuffix(!),
    And(!),
    BaseName(!),
    Call(!),
    Dir(!),
    Error(!),
    Eval,
    File,
    Filter,
    FilterOut,
    FindString,
    FirstWord,
    Flavor,
    If,
    Info,
    Join,
    LastWord,
    NotDir,
    Or,
    Origin,
    PatSubst,
    Realpath,
    Sort,
    Strip,
    Subst,
    Suffix,
    Value,
    Warning,
    Wildcard,
    Word,
    WordList,
    Words,
}

fn as_text(start: usize, end: usize) -> VariableAstNode {
    VariableAstNode {
        start, end,
        ty: VariableAstNodeTy::Text,
    }
}

/// There are no lexical errors when parsing Makefiles
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum LexicalError {}

/// Adapts an iterator over tokens to an iterator over spans
fn adapt_token_iterator(
    it: impl Iterator<Item = tokenizer::Token>,
) -> impl Iterator<Item = (usize, tokenizer::TokenType, usize)> {
    it.map(|token| (token.start, token.token_type, token.end))
}

#[cfg(test)]
mod test {
    use super::adapt_token_iterator;
    use super::lexer_grammar::MakefileLineParser;
    use super::Lexeme;
    use crate::tokenizer::iterator_to_token_stream;
    use crate::tokenizer::TokenType;

    fn simple_iterator(s: &'static str) -> impl Iterator<Item = (usize, TokenType, usize)> {
        adapt_token_iterator(iterator_to_token_stream(s.char_indices()))
    }

    #[test]
    fn simple_whitespace() {
        let res: Vec<Lexeme> = assert_ok!(MakefileLineParser::new().parse(simple_iterator("  \t")));
        assert_eq!(vec![Lexeme::Whitespace], res);
    }
}

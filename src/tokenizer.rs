//! Implements tokenization of the makefile
use std::iter::{Iterator, Peekable};

#[cfg(test)]
use proptest_derive::Arbitrary;

/// A token in a makefile
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Token {
    /// Offset from the start of parsing, as defined by whatever iterator was
    /// passed into `iterator_to_token_stream`
    pub offset: usize,
    /// The type of this token
    pub token_type: TokenType,
}

impl Token {
    pub fn new(offset: usize, token_type: TokenType) -> Self {
        Self { offset, token_type }
    }
}

/// Represents the type of a token. All lengths are in bytes, not characters.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TokenType {
    /// Just some regular text character
    Text(char),
    /// Whitespace
    Whitespace(char),
    /// An escaped character (character preceded by a \)
    EscapedCharacter(Option<char>),
    /// A variable assignment operator
    VariableAssign(VariableAssign),
    /// The start of a variable reference (a `$` token)
    VariableReference,
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

impl TokenType {
    /// Get the length of this token, in utf8 bytes
    pub fn len(self) -> usize {
        match self {
            TokenType::EscapedCharacter(Some(c)) => 1 + c.len_utf8(),
            TokenType::VariableAssign(v) => v.len(),
            TokenType::Colon(IsDoubleColon::Yes) => 2,
            TokenType::Text(c) => c.len_utf8(),
            TokenType::Whitespace(c) => c.len_utf8(),
            _ => 1,
        }
    }
}

/// Is the colon involved a double colon?
#[allow(missing_docs)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(Arbitrary))]
pub enum IsDoubleColon {
    No,
    Yes,
}

/// A type of variable assignment
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(Arbitrary))]
pub enum VariableAssign {
    /// Corresponds to `=`
    Recursive,
    /// Corresponds to `:=`
    Simple(IsDoubleColon),
    /// Corresponds to `+=`
    Append,
    /// Corresponds to `?=`
    Conditional,
    /// Corresponds to `!=`
    Bang,
}

impl VariableAssign {
    pub fn len(self) -> usize {
        match self {
            VariableAssign::Recursive => 1,
            VariableAssign::Simple(IsDoubleColon::Yes) => 3,
            _ => 2,
        }
    }
}

/// Internal iterator implementation
struct TokenStream<IT> {
    internal: IT,
}

impl<IT> Iterator for TokenStream<Peekable<IT>>
where
    IT: Iterator<Item = (usize, char)>,
{
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        let (start_idx, chr) = self.internal.next()?;

        macro_rules! token {
            ($ty:expr) => {
                Some(Token {
                    offset: start_idx,
                    token_type: $ty,
                })
            };
        };

        macro_rules! check_next {
            ($chr:expr, $matches:expr, $nomatch:expr) => {{
                if let Some((_, chr)) = self.internal.peek() {
                    if *chr == $chr {
                        self.internal.next();

                        $matches
                    } else {
                        $nomatch
                    }
                } else {
                    $nomatch
                }
            }};
        }

        if chr == '\n' {
            token!(TokenType::NewLine)
        } else if chr.is_whitespace() {
            token!(TokenType::Whitespace(chr))
        } else if chr == '\\' {
            token!(TokenType::EscapedCharacter(
                self.internal.next().map(|x| x.1)
            ))
        } else if chr == '$' {
            token!(TokenType::VariableReference)
        } else if chr == '(' {
            token!(TokenType::OpenParen)
        } else if chr == ')' {
            token!(TokenType::CloseParen)
        } else if chr == '{' {
            token!(TokenType::OpenBrace)
        } else if chr == '}' {
            token!(TokenType::CloseBrace)
        } else if chr == ':' {
            // This is tricky, since the token may be one of the following:
            //  :  :: := ::=

            let dcolon = check_next!(':', IsDoubleColon::Yes, IsDoubleColon::No);

            check_next!(
                '=',
                token!(TokenType::VariableAssign(VariableAssign::Simple(dcolon))),
                token!(TokenType::Colon(dcolon))
            )
        } else if chr == ';' {
            token!(TokenType::SemiColon)
        } else if chr == '%' {
            token!(TokenType::Percent)
        } else if chr == '+' {
            check_next!(
                '=',
                token!(TokenType::VariableAssign(VariableAssign::Append)),
                token!(TokenType::Text(chr))
            )
        } else if chr == '?' {
            check_next!(
                '=',
                token!(TokenType::VariableAssign(VariableAssign::Conditional)),
                token!(TokenType::Text(chr))
            )
        } else if chr == '!' {
            check_next!(
                '=',
                token!(TokenType::VariableAssign(VariableAssign::Bang)),
                token!(TokenType::Text(chr))
            )
        } else if chr == '=' {
            token!(TokenType::VariableAssign(VariableAssign::Recursive))
        } else if chr == '#' {
            token!(TokenType::CommentStart)
        } else {
            token!(TokenType::Text(chr))
        }
    }
}

/// Adapt an iterator over characters to an iterator over tokens
pub fn iterator_to_token_stream(
    it: impl Iterator<Item = (usize, char)>,
) -> impl Iterator<Item = Token> {
    TokenStream {
        internal: it.peekable(),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use proptest::prelude::*;

    fn flatten_stream(tokens: &[Token]) -> String {
        let mut buffer = String::with_capacity(tokens.len());

        for token in tokens {
            match token.token_type {
                TokenType::Whitespace(c) | TokenType::Text(c) => buffer.push(c),
                TokenType::EscapedCharacter(Some(c)) => {
                    buffer.push('\\');
                    buffer.push(c)
                }
                TokenType::EscapedCharacter(None) => buffer.push('\\'),
                TokenType::VariableAssign(VariableAssign::Recursive) => buffer.push_str("="),
                TokenType::VariableAssign(VariableAssign::Simple(IsDoubleColon::No)) => {
                    buffer.push_str(":=")
                }
                TokenType::VariableAssign(VariableAssign::Simple(IsDoubleColon::Yes)) => {
                    buffer.push_str("::=")
                }
                TokenType::VariableAssign(VariableAssign::Append) => buffer.push_str("+="),
                TokenType::VariableAssign(VariableAssign::Conditional) => buffer.push_str("?="),
                TokenType::VariableAssign(VariableAssign::Bang) => buffer.push_str("!="),
                TokenType::VariableReference => buffer.push('$'),
                TokenType::OpenParen => buffer.push('('),
                TokenType::CloseParen => buffer.push(')'),
                TokenType::OpenBrace => buffer.push('{'),
                TokenType::CloseBrace => buffer.push('}'),
                TokenType::Colon(IsDoubleColon::No) => buffer.push(':'),
                TokenType::Colon(IsDoubleColon::Yes) => buffer.push_str("::"),
                TokenType::SemiColon => buffer.push(';'),
                TokenType::Percent => buffer.push('%'),
                TokenType::NewLine => buffer.push('\n'),
                TokenType::CommentStart => buffer.push('#'),
            }
        }

        buffer
    }

    proptest! {
        #[test]
        fn round_trip_tokens(input in r"[=\\+!%: \t\n#$(){}¥ѨȺ🕴]+") {
            let parsed: Vec<Token> = iterator_to_token_stream(input.char_indices()).collect();
            let flat = flatten_stream(&parsed);

            prop_assert_eq!(input, flat);
        }
    }

    #[test]
    fn test_single_colon() {
        let input = ":";
        let parsed: Vec<Token> = iterator_to_token_stream(input.char_indices()).collect();

        assert_eq!(
            vec![Token::new(0, TokenType::Colon(IsDoubleColon::No))],
            parsed
        )
    }

    #[test]
    fn test_double_colon() {
        let input = "::";
        let parsed: Vec<Token> = iterator_to_token_stream(input.char_indices()).collect();

        assert_eq!(
            vec![Token::new(0, TokenType::Colon(IsDoubleColon::Yes))],
            parsed
        )
    }
}

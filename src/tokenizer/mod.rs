//! Implements tokenization of the makefile
use std::iter::{Iterator, Peekable};

// mod tokenizer_grammar;
mod text_matcher;

#[cfg(test)]
use proptest_derive::Arbitrary;

/// A token in a makefile
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Token {
    /// Offset from the start of parsing, as defined by whatever iterator was
    /// passed into `iterator_to_token_stream`
    pub start: usize,
    /// The type of this token
    pub token_type: TokenType,
    /// Final character
    pub end: usize,
}

impl Token {
    fn new(start: usize, token_type: TokenType, end: usize) -> Token {
        Token {
            start,
            token_type,
            end,
        }
    }
}

/// Represents the type of a token. All lengths are in bytes, not characters.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TokenType {
    /// A directive
    Directive(Directive),
    /// A builtin function name
    BuiltinFunction(BuiltinFunction),
    /// Some plain text
    Text,
    /// Whitespace characters
    Whitespace,
    /// An escaped character (character preceded by a \)
    EscapedCharacter(Option<char>),
    /// A variable assignment operator
    VariableAssign(VariableAssign),
    /// The start of a variable reference (a `$` token)
    VariableReference(VariableKind),
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
    /// A `"` character
    DoubleQuote,
    /// A `'` character
    SingleQuote,
    /// A `,` character
    Comma,
}

/// Is the colon involved a double colon?
#[allow(missing_docs)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(Arbitrary))]
pub enum IsDoubleColon {
    No,
    Yes,
}

/// Is the directive involved soft (i.e. are failures hard errors)?
#[allow(missing_docs)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(Arbitrary))]
pub enum IsSoft {
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

/// What kind of variable reference is this?
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(Arbitrary))]
pub enum VariableKind {
    /// It's a $ followed by a single character
    SingleCharacter,
    /// It's a $ followed by an open parenthesis
    OpenParen,
    /// It's a $ followed by an open brace
    OpenBrace,
    /// It's definitely unterminated. This happens if the character after the $
    /// is a whitespace character
    Unterminated,
}

/// A builtin function
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[allow(missing_docs)]
pub enum BuiltinFunction {
    Abspath,
    AddPrefix,
    AddSuffix,
    And,
    BaseName,
    Call,
    Dir,
    Error,
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

/// All the directives that may occur
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[allow(missing_docs)]
pub enum Directive {
    Export,
    Include(IsSoft),
    Load(IsSoft),
    UnExport,
    VPath,

    Else,
    EndIf,
    IfDef,
    IfEq,
    IfNDef,
    IfNEq,

    Define,
    Enddef,
}

/// Internal iterator implementation
struct TokenStream<IT> {
    internal: IT,
}

const SPECIAL_CHARACTERS: &'static str = "\\$(){}:;%+?!=#\"',";

impl<IT> Iterator for TokenStream<Peekable<IT>>
where
    IT: Iterator<Item = (usize, char)>,
{
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        let (start_idx, chr) = self.internal.next()?;
        let mut end_idx = start_idx + chr.len_utf8();

        macro_rules! consume_next {
            () => {
                match self.internal.next() {
                    Some((idx, chr)) => {
                        end_idx = idx + chr.len_utf8();
                        Some(chr)
                    }
                    None => None,
                }
            };
        };

        macro_rules! token {
            ($ty:expr) => {
                Some(Token {
                    start: start_idx,
                    end: end_idx,
                    token_type: $ty,
                })
            };
        };

        macro_rules! check_next {
            ($chr:expr, $matches:expr, $nomatch:expr) => {{
                if let Some((_, chr)) = self.internal.peek() {
                    if *chr == $chr {
                        consume_next!();

                        $matches
                    } else {
                        $nomatch
                    }
                } else {
                    $nomatch
                }
            }};
        }

        if chr == ' ' || chr == '\t' {
            // Make only considers ' ' and '\t' to be whitespace. All other
            // whitespace characters are interpreted as newlines.
            while let Some((_next_idx, chr)) = self.internal.peek() {
                let chr = *chr;
                if chr != ' ' && chr != '\t' {
                    break;
                }
                consume_next!();
            }
            token!(TokenType::Whitespace)
        } else if chr.is_whitespace() {
            token!(TokenType::NewLine)
        } else if chr == '\\' {
            match consume_next!() {
                None => token!(TokenType::EscapedCharacter(None)),
                Some(chr2) => token!(TokenType::EscapedCharacter(Some(chr2))),
            }
        } else if chr == '$' {
            if let Some((next_start, next)) = self.internal.peek() {
                // kill borrow of internal
                let next_start = *next_start;
                let next = *next;

                match next {
                    '$' => {
                        // If it's another $ sign, this isn't a variable reference but an "escaped" $
                        consume_next!();
                        Some(Token::new(next_start, TokenType::Text, end_idx))
                    }
                    '(' => {
                        consume_next!();
                        token!(TokenType::VariableReference(VariableKind::OpenParen))
                    }
                    '{' => {
                        consume_next!();
                        token!(TokenType::VariableReference(VariableKind::OpenBrace))
                    }
                    ' ' | '\t' => {
                        // We need this branch here explicitly to catch these
                        // "whitespace" characters which are treated as variable
                        // name. In practice I'm pretty sure it's impossible to
                        // actually set these variables ("$ " and "$\t") to
                        // anything, but we need to parse references to them
                        // correctly and thus need to catch these names before
                        // the is_whitespace case.
                        // See tests/makefiles/single_dollar.mk
                        consume_next!();
                        token!(TokenType::VariableReference(VariableKind::SingleCharacter))
                    }
                    c if c.is_whitespace() => {
                        // This is some sort of line break, emit as just a raw $
                        // See tests/makefiles/single_dollar.mk
                        token!(TokenType::Text)
                    }
                    '#' => {
                        // Another "end of line" case
                        // See tests/makefiles/comment_intro_variable.mk
                        token!(TokenType::Text)
                    }
                    _ => {
                        // Note that this case implicitly picks up backslashes!
                        // This is intentional, and matches Make's behavior
                        // See tests/makefile/var_name_backslash.mk (run with
                        // --warn-undefined-variables to get Make to print the
                        // names of the variables being referenced)
                        consume_next!();
                        token!(TokenType::VariableReference(VariableKind::SingleCharacter))
                    }
                }
            } else {
                // We saw a $ at the end of input, which means it's treated as just text
                // (see tests/makefiles/single_dollar.mk)
                token!(TokenType::Text)
            }
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

            let ty = check_next!(
                '=',
                TokenType::VariableAssign(VariableAssign::Simple(dcolon)),
                TokenType::Colon(dcolon)
            );
            token!(ty)
        } else if chr == ';' {
            token!(TokenType::SemiColon)
        } else if chr == '%' {
            token!(TokenType::Percent)
        } else if chr == '+' {
            let ty = check_next!(
                '=',
                TokenType::VariableAssign(VariableAssign::Append),
                TokenType::Text
            );
            token!(ty)
        } else if chr == '?' {
            let ty = check_next!(
                '=',
                TokenType::VariableAssign(VariableAssign::Conditional),
                TokenType::Text
            );
            token!(ty)
        } else if chr == '!' {
            let ty = check_next!(
                '=',
                TokenType::VariableAssign(VariableAssign::Bang),
                TokenType::Text
            );
            token!(ty)
        } else if chr == '=' {
            token!(TokenType::VariableAssign(VariableAssign::Recursive))
        } else if chr == '#' {
            token!(TokenType::CommentStart)
        } else if chr == '\'' {
            token!(TokenType::SingleQuote)
        } else if chr == '"' {
            token!(TokenType::DoubleQuote)
        } else if chr == ',' {
            token!(TokenType::Comma)
        } else {
            // Attempt to match using the generated matcher
            let (updated_end, matched) = text_matcher::do_match(end_idx, chr, &mut self.internal);
            end_idx = updated_end;
            if matched.is_some() {
                return token!(matched.unwrap());
            }
            while let Some((_next_idx, chr2)) = self.internal.peek() {
                if chr2.is_whitespace() || SPECIAL_CHARACTERS.find(*chr2).is_some() {
                    break;
                }
                consume_next!();
            }
            return token!(TokenType::Text);
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

    fn flatten_stream(original_string: &str, tokens: &[Token]) -> String {
        let mut buffer = String::with_capacity(tokens.len());

        let mut last_end = 0;

        macro_rules! check_and_push {
            ($tok:expr, $s:expr) => {{
                let token = $tok;
                let original = &original_string[token.start..token.end];
                let s = $s;
                assert!(s.eq_ignore_ascii_case(original), "string round tripped");
                buffer.push_str(original);
            }};
        };

        for token in tokens {
            match token.token_type {
                TokenType::Whitespace => {
                    let slice = &original_string[token.start..token.end];
                    for chr in slice.chars() {
                        assert!(chr.is_whitespace())
                    }
                    buffer.push_str(slice);
                }
                TokenType::Text => {
                    let slice = &original_string[token.start..token.end];

                    // Special case: an "escaped" dollar character
                    if slice == "$" && last_end != token.start {
                        buffer.push_str("$$");
                    } else {
                        buffer.push_str(slice);
                    }
                }
                TokenType::EscapedCharacter(Some(c)) => {
                    buffer.push('\\');
                    buffer.push(c)
                }
                TokenType::EscapedCharacter(None) => buffer.push('\\'),
                TokenType::Directive(directive) => match directive {
                    Directive::Export => check_and_push!(token, "Export"),
                    Directive::Include(soft) => match soft {
                        IsSoft::Yes => check_and_push!(token, "-include"),
                        IsSoft::No => check_and_push!(token, "include"),
                    },
                    Directive::Load(soft) => match soft {
                        IsSoft::Yes => check_and_push!(token, "-load"),
                        IsSoft::No => check_and_push!(token, "load"),
                    },
                    Directive::UnExport => check_and_push!(token, "UnExport"),
                    Directive::VPath => check_and_push!(token, "VPath"),

                    Directive::Else => check_and_push!(token, "Else"),
                    Directive::EndIf => check_and_push!(token, "EndIf"),
                    Directive::IfDef => check_and_push!(token, "IfDef"),
                    Directive::IfEq => check_and_push!(token, "IfEq"),
                    Directive::IfNDef => check_and_push!(token, "IfNDef"),
                    Directive::IfNEq => check_and_push!(token, "IfNeq"),

                    Directive::Define => check_and_push!(token, "Define"),
                    Directive::Enddef => check_and_push!(token, "Enddef"),
                },
                TokenType::BuiltinFunction(function) => match function {
                    BuiltinFunction::Abspath => check_and_push!(token, "Abspath"),
                    BuiltinFunction::AddPrefix => check_and_push!(token, "AddPrefix"),
                    BuiltinFunction::AddSuffix => check_and_push!(token, "AddSuffix"),
                    BuiltinFunction::And => check_and_push!(token, "And"),
                    BuiltinFunction::BaseName => check_and_push!(token, "BaseName"),
                    BuiltinFunction::Call => check_and_push!(token, "Call"),
                    BuiltinFunction::Dir => check_and_push!(token, "Dir"),
                    BuiltinFunction::Error => check_and_push!(token, "Error"),
                    BuiltinFunction::Eval => check_and_push!(token, "Eval"),
                    BuiltinFunction::File => check_and_push!(token, "File"),
                    BuiltinFunction::Filter => check_and_push!(token, "Filter"),
                    BuiltinFunction::FilterOut => check_and_push!(token, "FilterOut"),
                    BuiltinFunction::FindString => check_and_push!(token, "FindString"),
                    BuiltinFunction::FirstWord => check_and_push!(token, "FirstWord"),
                    BuiltinFunction::Flavor => check_and_push!(token, "Flavor"),
                    BuiltinFunction::If => check_and_push!(token, "If"),
                    BuiltinFunction::Info => check_and_push!(token, "Info"),
                    BuiltinFunction::Join => check_and_push!(token, "Join"),
                    BuiltinFunction::LastWord => check_and_push!(token, "LastWord"),
                    BuiltinFunction::NotDir => check_and_push!(token, "NotDir"),
                    BuiltinFunction::Or => check_and_push!(token, "Or"),
                    BuiltinFunction::Origin => check_and_push!(token, "Origin"),
                    BuiltinFunction::PatSubst => check_and_push!(token, "PatSubst"),
                    BuiltinFunction::Realpath => check_and_push!(token, "Realpath"),
                    BuiltinFunction::Sort => check_and_push!(token, "Sort"),
                    BuiltinFunction::Strip => check_and_push!(token, "Strip"),
                    BuiltinFunction::Subst => check_and_push!(token, "Subst"),
                    BuiltinFunction::Suffix => check_and_push!(token, "Suffix"),
                    BuiltinFunction::Value => check_and_push!(token, "Value"),
                    BuiltinFunction::Warning => check_and_push!(token, "Warning"),
                    BuiltinFunction::Wildcard => check_and_push!(token, "Wildcard"),
                    BuiltinFunction::Word => check_and_push!(token, "Word"),
                    BuiltinFunction::WordList => check_and_push!(token, "WordList"),
                    BuiltinFunction::Words => check_and_push!(token, "Words"),
                },
                TokenType::VariableAssign(kind) => match kind {
                    VariableAssign::Recursive => buffer.push_str("="),
                    VariableAssign::Simple(IsDoubleColon::No) => buffer.push_str(":="),
                    VariableAssign::Simple(IsDoubleColon::Yes) => buffer.push_str("::="),
                    VariableAssign::Append => buffer.push_str("+="),
                    VariableAssign::Conditional => buffer.push_str("?="),
                    VariableAssign::Bang => buffer.push_str("!="),
                },
                TokenType::VariableReference(kind) => match kind {
                    VariableKind::SingleCharacter => {
                        let span = &original_string[token.start..token.end];
                        buffer.push_str(span);
                    }
                    VariableKind::OpenParen => buffer.push_str("$("),
                    VariableKind::OpenBrace => buffer.push_str("${"),
                    VariableKind::Unterminated => buffer.push('$'),
                },
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
                TokenType::SingleQuote => buffer.push('\''),
                TokenType::DoubleQuote => buffer.push('"'),
                TokenType::Comma => buffer.push(','),
            }

            last_end = token.end;
        }

        buffer
    }

    proptest! {
        #[test]
        fn round_trip_tokens(input in r"[[:alpha:]\-=\\+!%: \t\n#$(){}¥ѨȺ🕴]+") {
            let parsed: Vec<Token> = iterator_to_token_stream(input.char_indices()).collect();
            let flat = flatten_stream(&input, &parsed);

            prop_assert_eq!(input, flat);
        }
    }

    #[test]
    fn single_colon() {
        let input = ":";
        let parsed: Vec<Token> = iterator_to_token_stream(input.char_indices()).collect();

        assert_eq!(
            vec![Token::new(0, TokenType::Colon(IsDoubleColon::No), 1)],
            parsed
        );
    }

    #[test]
    fn double_colon() {
        let input = "::";
        let parsed: Vec<Token> = iterator_to_token_stream(input.char_indices()).collect();

        assert_eq!(
            vec![Token::new(0, TokenType::Colon(IsDoubleColon::Yes), 2)],
            parsed
        );
    }

    #[test]
    fn two_newlines() {
        let input = "\n\n";
        let parsed: Vec<Token> = iterator_to_token_stream(input.char_indices()).collect();

        assert_eq!(
            vec![
                Token::new(0, TokenType::NewLine, 1),
                Token::new(1, TokenType::NewLine, 2)
            ],
            parsed
        );
    }

    #[test]
    fn prefix_of_keyword() {
        let input = "ife";
        let parsed: Vec<Token> = iterator_to_token_stream(input.char_indices()).collect();

        assert_eq!(vec![Token::new(0, TokenType::Text, 3)], parsed)
    }

    #[test]
    fn suffix_of_keyword() {
        let input = "ifeqa";
        let parsed: Vec<Token> = iterator_to_token_stream(input.char_indices()).collect();

        assert_eq!(vec![Token::new(0, TokenType::Text, 5)], parsed)
    }

    #[test]
    fn recognize_ifeq_at_start() {
        let input = "ifeq()";
        let parsed: Vec<Token> = iterator_to_token_stream(input.char_indices()).collect();

        assert_eq!(
            vec![
                Token::new(0, TokenType::Directive(Directive::IfEq), 4),
                Token::new(4, TokenType::OpenParen, 5),
                Token::new(5, TokenType::CloseParen, 6)
            ],
            parsed
        )
    }

    #[test]
    fn recognize_else() {
        crate::test::setup();
        let input = "else";
        let parsed: Vec<Token> = iterator_to_token_stream(input.char_indices()).collect();

        assert_eq!(
            vec![Token::new(0, TokenType::Directive(Directive::Else), 4)],
            parsed
        );
    }
}

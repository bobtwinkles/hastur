//! Parser for target lines
use super::variable::VariableAction;
use crate::ast::{AstChildren, AstNode};
use crate::evaluated::BlockSpan;
use crate::parsers::ast::parse_ast;
use crate::parsers::makefile_whitespace;
use crate::{Database, Engine, FileName, NameCache, ParseErrorKind, VariableName};
use nom::IResult;

#[cfg(test)]
mod test;

/// What action to take in response to this line
#[derive(Clone, Debug)]
pub(crate) enum Action {
    /// The start of a new rule
    NewRule {
        targets: Vec<FileName>,
        deps: AstNode,
    },

    /// A target specific variable
    TargetVariable {
        targets: Vec<String>,
        variable_action: VariableAction,
    },
}

impl<'a> crate::parsers::ParserState<'a> {
    pub(crate) fn handle_target_action(
        &mut self,
        engine: &mut Engine,
        action: Action,
    ) -> Result<(), ParseErrorKind> {
        match action {
            e => unimplemented!("Unimplemented action {:?}", e),
        }
        Ok(())
    }
}

/// Represents the reason why we ended the "makefile word"
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum MWordEnd {
    /// End of the line was reached
    EOL,
    /// A static word that can't be expanded was encountered
    Static,
    /// A variable expansion
    Variable,
    /// A colon was encountered
    Colon,
    /// A double colon was encountered
    DColon,
    /// A semicolon was encountered
    Semicolon,
    /// A variable assignment was encountered
    VarAssign,
}

pub(crate) fn parse_line<'a>(
    i: BlockSpan<'a>,
    names: &mut NameCache,
    database: &Database,
) -> IResult<BlockSpan<'a>, (Database, Action), ParseErrorKind> {
    let (i, line_ast) = parse_ast(i)?;
    // Grab the list of nodes. If it's a concatenation of multiple expansions,
    // then we're going to iterate over them. Otherwise, we just pretend the one
    // node we have is actually a list.
    let nodes = match line_ast.children() {
        AstChildren::Concat(children) => &children,
        _ => std::slice::from_ref(&line_ast),
    };
    let mut database = database.clone();
    for i in nodes {
        let expanded_node = {
            let (newdb, res) = i.eval(names, &database);
            database = newdb;
            res
        };
        eprintln!("{:?}", expanded_node);
    }

    Ok((i, (database, unimplemented!())))
}

fn next_mword<'a>(
    i: BlockSpan<'a>,
) -> IResult<BlockSpan<'a>, (BlockSpan<'a>, MWordEnd), ParseErrorKind> {
    use nom::{InputIter, InputTake, Offset, Slice};
    let start_i = i;

    // Skip any leading whitespace
    let (i, _) = makefile_whitespace(i)?;

    // Early out on simple cases
    if let Ok((i, (cap, end_reason))) = pe_fix!(
        i,
        alt!(
            tag!(";") =>   { |c| (c, MWordEnd::Semicolon) } |
            tag!("=") =>   { |c| (c, MWordEnd::VarAssign) } |
            tag!(":=") =>  { |c| (c, MWordEnd::VarAssign) } |
            tag!("::=") => { |c| (c, MWordEnd::VarAssign) } |
            tag!("::") =>  { |c| (c, MWordEnd::DColon) } |
            tag!(":") =>   { |c| (c, MWordEnd::Colon) } |
            tag!("+=") =>  { |c| (c, MWordEnd::VarAssign) } |
            tag!("?=") =>  { |c| (c, MWordEnd::VarAssign) } |
            tag!("!=") =>  { |c| (c, MWordEnd::VarAssign) }
        )
    ) {
        return Ok((i, (cap, end_reason)));
    };

    // More complex cases
    let mut char_iter = i.iter_indices().peekable();
    let mut stop_index = 0;
    let mut end_reason = MWordEnd::Static;

    while let Some((idx, chr)) = char_iter.next() {
        stop_index = idx + 1;

        if chr == ' ' || chr == '\t' || chr == '=' {
            end_reason = MWordEnd::Static;
            stop_index = idx;
            break;
        }

        if chr == ':' {
            unimplemented!("DOS path handling");
        }

        if chr == '$' {
            // This is probably a variable reference of some sort
            if let Some((idx, chr)) = char_iter.next() {
                stop_index = idx + 1;
                if chr == '$' {
                    // Consume the character
                    // This is just a double '$', does not end the word
                    continue;
                }
                end_reason = MWordEnd::Variable;

                // Scan forward until we find the matching end brace
                let brace_end = if chr == '(' {
                    ')'
                } else if chr == '{' {
                    '}'
                } else {
                    // Single character variable reference
                    continue;
                };

                let mut count = 1;
                while let Some((idx, chr2)) = char_iter.next() {
                    stop_index = idx + 1;
                    if chr2 == chr {
                        count += 1;
                    } else if chr2 == brace_end {
                        count -= 1;
                    }
                    if count == 0 {
                        break;
                    }
                }
                continue;
            } else {
                end_reason = MWordEnd::Static;
                break;
            }
            continue;
        }

        if chr == '\\' {
            if let Some((_, chr)) = char_iter.peek() {
                let chr = *chr;
                if chr == ':' || chr == ';' || chr == '=' || chr == '\\' {
                    // This is escaping something, skip forward
                    char_iter.next();
                }
            }
            continue;
        }

        if chr == '?' || chr == '+' {
            // If we see an assignment-like thing, the word is over
            if let Some((_, chr)) = char_iter.peek() {
                // This subtraction is safe since if we were on the first character,
                // the early-out above would have caught the "?="
                stop_index -= 1;
                if *chr == '=' {
                    break;
                }
            }
        }
    }

    let (i, _) = i.take_split(stop_index);

    Ok((i, (start_i.slice(..(start_i.offset(&i))), end_reason)))
}

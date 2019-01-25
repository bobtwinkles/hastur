//! Parser for variable definitions

// #SPC-P-Variable
use super::{error_out, makefile_whitespace};
use crate::ast;
use crate::eval::Flavor;
use crate::eval::VariableParameters;
use crate::evaluated::BlockSpan;
use crate::parsers::ast::parse_ast;
use crate::ParseErrorKind;
use crate::VariableName;
use nom::{Err, ErrorKind, IResult};

#[cfg(test)]
mod test;

/// Attempt to parse a variable reference in the context of a given database
pub(super) fn parse_line<'a>(
    i: BlockSpan<'a>,
    db: &mut crate::Database,
) -> IResult<BlockSpan<'a>, (VariableName, VariableParameters), ParseErrorKind> {
    use nom::Slice;

    let (i, _) = makefile_whitespace(i)?; // Skip to the first non-whitespace token
    let full_block = i;

    macro_rules! next_safe {
        ($it:expr, $op:expr) => {
            match ($it).next() {
                Some(v) => v,
                None => {
                    // We reached the end of the block span without finding an equals sign,
                    // this isn't a variable expansion
                    return error_out(i, ParseErrorKind::InternalFailure($op));
                }
            }
        };
    }

    let mut seen_whitespace = false;
    let (flavor, name_end, value_start) = {
        use nom::InputIter;

        let mut prev;
        let mut curr = ' ';
        let mut it = i.iter_indices();
        let eq_idx = loop {
            prev = curr;
            let (curr_index, next) = next_safe!(it, "outer search");
            curr = next;

            if curr == '$' {
                // This is probably a variable reference. Don't parse it into an AST now,
                // just skip over it and we'll pick it up later during line expansion
                let (_, open) = next_safe!(it, "var open");
                let close = match open {
                    '(' => ')',
                    '{' => '}',
                    _ => {
                        // It's not an open or close so this isn't a complex variable reference. Cary on parsing.
                        curr = open;
                        continue;
                    }
                };
                let mut count = 0;
                // The actual skip loop
                while curr != close && count > 0 {
                    let (_, next) = match it.next() {
                        Some(v) => v,
                        None => {
                            return error_out(i, ParseErrorKind::UnternimatedVariable);
                        }
                    };
                    curr = next;

                    if curr == open {
                        count += 1;
                    } else if curr == close {
                        count -= 1;
                    }
                }

                continue;
            }
            if curr.is_whitespace() {
                seen_whitespace = true;
            }
            if curr == '=' {
                break curr_index;
            }
        };

        let (flavor, name_end, value_start) = match prev {
            ':' => {
                // #SPC-P-Variable.simple
                // This catches both GNU style (:=) and POSIX style (::=)
                // assignments

                (Flavor::Simple, eq_idx - 2, eq_idx + 1)
            }
            '+' => {
                // #SPC-P-Variable.append_set
                (Flavor::Append, eq_idx - 2, eq_idx + 1)
            }
            '?' => {
                // #SPC-P-Variable.conditional_set
                (Flavor::Conditional, eq_idx - 2, eq_idx + 1)
            }
            '!' => {
                // #SPC-P-Variable.shell_set
                (Flavor::Shell, eq_idx - 2, eq_idx + 1)
            }
            c => {
                if !c.is_whitespace() && seen_whitespace {
                    // We skipped over some whitespace, but then got some garbage
                    // character before the = operator. This is not an assignment
                    return error_out(
                        i,
                        ParseErrorKind::InternalFailure(
                            "seen whitespace but not a valid variable assign",
                        ),
                    );
                } else {
                    // #SPC-P-Variable.recursive
                    (Flavor::Recursive, eq_idx - 1, eq_idx + 1)
                }
            }
        };

        (flavor, name_end, value_start)
    };

    let name_segment = full_block.slice(..name_end);
    let (value_segment, _) = makefile_whitespace(full_block.slice(value_start..))?;

    let (_, name_ast) = parse_ast(name_segment)?;
    let (post_value, mut value_ast) = parse_ast(value_segment)?;

    let variable_name = name_ast.eval(db).into_string();
    let variable_name = db.intern_variable_name(variable_name);

    if flavor == Flavor::Simple {
        // Evaluate the value AST
        let contents = value_ast.eval(db);
        let location = value_segment.segments().next().unwrap().location().clone();
        value_ast = ast::preevaluated(location.into(), contents);
    }

    // TODO: the origin reported here is not necessarily correct. We need to
    // inspect ask the block span whether it comes from an $(eval) or a file
    Ok((
        post_value,
        (
            variable_name,
            VariableParameters::new(value_ast, flavor, crate::eval::Origin::File),
        ),
    ))
}

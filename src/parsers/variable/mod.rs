//! Parser for variable definitions

// #SPC-P-Variable
use super::{error_out, makefile_whitespace};
use crate::ast;
use crate::eval::Flavor;
use crate::eval::VariableParameters;
use crate::evaluated::BlockSpan;
use crate::parsers::ast::parse_ast;
use crate::{Engine, ParseErrorKind, VariableName};
use nom::IResult;

#[cfg(test)]
mod test;

#[derive(Copy, Clone, Debug, PartialEq)]
enum AssignmentType {
    Recursive,
    Conditional,
    Simple,
    Append,
    Bang,
}

/// What action to apply to the variable
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Action {
    /// (re)define the variable
    Define(VariableParameters),
    /// Append to the specified variable
    Append(ast::AstNode),
}

/// Block of modifiers
#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub(crate) struct Modifiers {
    pub export: bool,
    pub mod_override: bool,
    pub private: bool,
    pub define: bool,
    pub undefine: bool,
}

/// What action to take with the variable
#[derive(Clone, Debug, PartialEq)]
pub(crate) struct VariableAction {
    pub name: VariableName,
    pub modifiers: Modifiers,
    pub action: Action,
}

impl<'a> crate::parsers::ParserState<'a> {
    pub(crate) fn handle_global_variable_action(
        &mut self,
        engine: &mut Engine,
        action: VariableAction,
    ) -> Result<(), ParseErrorKind> {
        if action.modifiers.export
            || action.modifiers.mod_override
            || action.modifiers.private
            || action.modifiers.define
            || action.modifiers.undefine
        {
            unimplemented!("Handling of variable with modifiers {:?}", action.modifiers);
        }

        match action.action {
            Action::Define(parameters) => {
                engine.database = engine.database.set_variable(action.name, parameters);
            }
            Action::Append(node) => match engine.database.get_variable(action.name) {
                Some(var) => {
                    let mut parameters = var.value.clone();
                    parameters.unexpanded_value = ast::collapsing_concat(
                        node.location(),
                        vec![parameters.unexpanded_value, node],
                    );
                    engine.database = engine.database.set_variable(action.name, parameters)
                }
                None => {
                    engine.database = engine.database.set_variable(
                        action.name,
                        VariableParameters::new(node, Flavor::Recursive, crate::eval::Origin::File),
                    )
                }
            },
        }

        Ok(())
    }
}

/// Attempt to parse a variable reference in the context of a given database
pub(crate) fn parse_line<'a>(
    i: BlockSpan<'a>,
    names: &mut crate::NameCache,
    db: &crate::Database,
) -> IResult<BlockSpan<'a>, (crate::Database, VariableAction), ParseErrorKind> {
    use nom::Slice;

    #[derive(Debug, Copy, Clone)]
    enum Modifier {
        Export,
        Override,
        Private,
        Define,
        Undefine,
    }

    let (mut i, _) = makefile_whitespace(i)?; // Skip to the first non-whitespace token

    if i.len() == 0 {
        return error_out(i, ParseErrorKind::InternalFailure("no content on line"));
    }

    // Match modifiers
    let mut modifiers: Modifiers = Default::default();
    loop {
        // If parsing as a variable definition succeeds, we're done
        eprintln!("Parsing iteration {:?}", i.into_string());
        match parse_variable_assignment(i, modifiers, names, db) {
            Ok(v) => return Ok(v),
            Err(e) => {
                match e.clone().into_error_kind() {
                    nom::ErrorKind::Custom(ParseErrorKind::UnternimatedVariable) => return Err(e),
                    _ => {
                        // We don't propagate any other kind of error
                    }
                }
            }
        }

        // Otherwise, try to match against a keyword
        let (new_i, modifier) = match terminated!(
            i,
            alt!(
                pe_complete!(pe_fix!(tag_no_case!("define"))) => { |_| Modifier::Define } |
                pe_complete!(pe_fix!(tag_no_case!("export"))) => { |_| Modifier::Export }  |
                pe_complete!(pe_fix!(tag_no_case!("override"))) => { |_| Modifier::Override } |
                pe_complete!(pe_fix!(tag_no_case!("private"))) => { |_| Modifier::Private } |
                pe_complete!(pe_fix!(tag_no_case!("undefine"))) => { |_| Modifier::Undefine }
            ),
            makefile_whitespace
        ) {
            Ok(b) => b,
            Err(_) => {
                // We didn't match a modifier or a variable assignment,
                // this isn't an assignment
                return error_out(i, ParseErrorKind::InternalFailure("not an assignment"));
            }
        };
        eprintln!(
            "Advancing from {:?} to {:?}",
            i.into_string(),
            new_i.into_string()
        );
        i = new_i;
        match std::dbg!(modifier) {
            Modifier::Export => modifiers.export = true,
            Modifier::Override => modifiers.mod_override = true,
            Modifier::Private => modifiers.private = true,
            Modifier::Define => {
                modifiers.define = true;
                // Try to parse this variable normally
                if let Ok((suffix, (variable_name, assignment_type))) = parse_assignment_operator(i)
                {
                    if let Ok((trailing, _)) = makefile_whitespace(suffix) {
                        if trailing.len() == 0 {
                            // TODO: propagate sensitivity from the name?
                            let (_, name_ast) = parse_ast(variable_name)?;
                            let (db, variable_name) = name_ast.eval(names, db);
                            let variable_name = variable_name.into_string().trim().into();
                            let variable_name = names.intern_variable_name(variable_name);
                            return Ok((
                                trailing,
                                (
                                    db,
                                    VariableAction {
                                        name: variable_name,
                                        modifiers: modifiers,
                                        action: match assignment_type {
                                            AssignmentType::Append => Action::Append(ast::empty()),
                                            v => Action::Define(VariableParameters::new(
                                                ast::empty(),
                                                match v {
                                                    AssignmentType::Simple => Flavor::Simple,
                                                    AssignmentType::Conditional => {
                                                        Flavor::Conditional
                                                    }
                                                    AssignmentType::Recursive => Flavor::Recursive,
                                                    AssignmentType::Bang => Flavor::Shell,
                                                    AssignmentType::Append => unreachable!(),
                                                },
                                                crate::eval::Origin::File,
                                            )),
                                        },
                                    },
                                ),
                            ));
                        }
                    }
                }
                // Failing that, just assume the whole thing is one big recursive variable name
                let variable_name = names.intern_variable_name(i.into_string().trim().into());
                let rest = i.slice(i.len()..);
                return Ok((
                    rest,
                    (
                        db.clone(),
                        VariableAction {
                            name: variable_name,
                            modifiers: modifiers,
                            action: Action::Define(VariableParameters::new(
                                ast::empty(),
                                Flavor::Recursive,
                                crate::eval::Origin::File,
                            )),
                        },
                    ),
                ));
            }
            Modifier::Undefine => {
                modifiers.undefine = true;
                unimplemented!("undefine modifier");
            }
        }
    }
}

/// Parse a span, returning the captured variable name and the type of
/// assignment on success. The remaining content will start directly after the
/// equals operator
fn parse_assignment_operator<'a>(
    i: BlockSpan<'a>,
) -> IResult<BlockSpan<'a>, (BlockSpan<'a>, AssignmentType), ParseErrorKind> {
    let (i, variable_name) = match take_till!(i, |c: char| c.is_whitespace()
        || c == ':'
        || c == '='
        || c == '!'
        || c == '?'
        || c == '+')
    {
        Ok(v) => v,
        Err(_) => return error_out(i, ParseErrorKind::InternalFailure("name not captured")),
    };

    eprintln!(
        "Captured variable name {:?}, {:?} left to parse",
        variable_name.into_string(),
        i.into_string()
    );

    let (i, _) = makefile_whitespace(i)?;

    let (i, assignment_type) = pe_fix!(
        i,
        alt!(
            pe_complete!(pe_fix!(tag!("="))) => { |_| { AssignmentType::Recursive }} |
            pe_complete!(pe_fix!(tag!("::="))) => { |_| { AssignmentType::Simple }} |
            pe_complete!(pe_fix!(tag!(":="))) => { |_| { AssignmentType::Simple }} |
            pe_complete!(pe_fix!(tag!("?="))) => { |_| { AssignmentType::Conditional }} |
            pe_complete!(pe_fix!(tag!("!="))) => { |_| { AssignmentType::Bang }} |
            pe_complete!(pe_fix!(tag!("+="))) => { |_| { AssignmentType::Append }}
        )
    )?;

    let (i, _) = makefile_whitespace(i)?;

    Ok((i, (variable_name, assignment_type)))
}

fn parse_variable_assignment<'a>(
    i: BlockSpan<'a>,
    modifiers: Modifiers,
    names: &mut crate::NameCache,
    db: &crate::Database,
) -> IResult<BlockSpan<'a>, (crate::Database, VariableAction), ParseErrorKind> {
    let (i, (name_segment, assignment_type)) = parse_assignment_operator(i)?;
    let (value_segment, _) = makefile_whitespace(i)?;

    let (_, name_ast) = parse_ast(name_segment)?;
    let (post_value, mut value_ast) = parse_ast(value_segment)?;

    let (mut db, variable_name) = name_ast.eval(names, db);
    let variable_name = variable_name.into_string().trim().into();
    let variable_name = names.intern_variable_name(variable_name);

    if assignment_type == AssignmentType::Simple {
        // Evaluate the value AST
        let (new_db, contents) = value_ast.eval(names, &db);
        db = new_db;
        let location = value_segment
            .location()
            .expect("value segment should have nonzero length");
        value_ast = ast::preevaluated(location.into(), contents);
    }

    // TODO: the origin reported here is not necessarily correct. We need to
    // inspect ask the block span whether it comes from an $(eval) or a file.
    // We also need to correctly propagate sensitivities
    Ok((
        post_value,
        (
            db,
            VariableAction {
                name: variable_name,
                modifiers: modifiers,
                action: match assignment_type {
                    AssignmentType::Append => Action::Append(value_ast),
                    v => Action::Define(VariableParameters::new(
                        value_ast,
                        match v {
                            AssignmentType::Simple => Flavor::Simple,
                            AssignmentType::Conditional => Flavor::Conditional,
                            AssignmentType::Recursive => Flavor::Recursive,
                            AssignmentType::Bang => Flavor::Shell,
                            AssignmentType::Append => unreachable!(),
                        },
                        crate::eval::Origin::File,
                    )),
                },
            },
        ),
    ))
}

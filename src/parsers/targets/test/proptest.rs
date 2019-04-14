//! Property-based testing for target parser

use crate::ast;
use crate::parsers::targets::{parse_line, Action};
use crate::parsers::test::create_span;
use crate::source_location::LocatedString;
use crate::NameCache;
use proptest::prelude::*;

fn arb_file_name(
    pre_separator: bool,
    using_command: bool,
) -> impl Strategy<Value = (String, String)> {
    // TODO: add ` ` here once parse_file_seq supports it
    if pre_separator {
        "[a\\\\:;]+"
    } else {
        "[a\\\\;]+"
    }
    .prop_map(move |x| {
        let mut var_name_buffer = String::with_capacity(x.capacity());
        let mut output_buffer = String::new();
        for c in x.chars() {
            if c == ':' {
                if pre_separator {
                    // Only colons before the separator are escaped
                    output_buffer.push('\\');
                    output_buffer.push(c);
                } else {
                    output_buffer.push(c)
                }
            } else if c == ';' {
                output_buffer.push_str(r"\\\");
                output_buffer.push(c);
                if pre_separator {
                    var_name_buffer.push('\\');
                }
            } else if c == '\\' {
                output_buffer.push('\\');
                output_buffer.push(c);
            } else {
                output_buffer.push(c);
            }
            var_name_buffer.push(c);
        }
        (var_name_buffer, output_buffer)
    })
}

fn arb_action() -> impl Strategy<Value = (Action, String, NameCache)> {
    use proptest::collection::vec as prop_vec;

    prop::option::of(Just("b"))
        .prop_flat_map(|initial_command| {
            (
                prop_vec(arb_file_name(true, initial_command.is_some()), 1..3),
                prop_vec(arb_file_name(false, initial_command.is_some()), 1..3),
                prop::bool::ANY,
                Just(initial_command),
                Just(NameCache::default()),
            )
        })
        .prop_map(|(targets, deps, dcolon, initial_command, mut name_cache)| {
            let mut output_buffer = String::new();
            for target in targets.iter() {
                output_buffer.push_str(&target.1);
                output_buffer.push_str(" ");
            }

            output_buffer.push(':');
            if dcolon {
                output_buffer.push(':');
            }

            for dep in deps.iter() {
                output_buffer.push_str(" ");
                output_buffer.push_str(&dep.1);
            }

            let initial_command = match initial_command {
                Some(v) => {
                    output_buffer.push(';');
                    let command_start = output_buffer.chars().count();
                    output_buffer.push_str(&v);
                    Some(ast::constant(LocatedString::test_new(
                        1,
                        1 + command_start as u32,
                        v,
                    )))
                }
                None => None,
            };

            let targets = targets
                .into_iter()
                .map(|v| name_cache.intern_file_name(v.0.into()))
                .collect();
            let deps = deps
                .into_iter()
                .map(|v| name_cache.intern_file_name(v.0.into()))
                .collect();

            (
                Action::NewRule {
                    targets,
                    deps,
                    double_colon: dcolon,
                    initial_command,
                },
                output_buffer,
                name_cache,
            )
        })
}

proptest![

    #[test]
    #[ignore]
    fn roundtrip_action((action, input, mut names) in arb_action()) {
        let block = create_span(&input);
        let mut engine = Default::default();
        match parse_line(block.span(), &mut names, &mut engine) {
            Ok((_, action_prime)) => {
                prop_assert_eq!(action_prime, action);
            }
            Err(e) => {
                prop_assert!(false, "Parsing failed {:?}", e);
            }
        }
    }
];

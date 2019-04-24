# Generates a Rust file which can parse the various directives and builtin
# functions

import os

directives = {
    "-include": "Include(IsSoft::Yes)",
    "-load": "Load(IsSoft::Yes)",
    "export": "Export",
    "include": "Include(IsSoft::No)",
    "load": "Load(IsSoft::No)",
    "sinclude": "Include(IsSoft::Yes)",
    "unexport": "UnExport",
    "vpath": "VPath",

    "else": "Else",
    "endif": "EndIf",
    "ifdef": "IfDef",
    "ifeq": "IfEq",
    "ifndef": "IfNDef",
    "ifneq": "IfNEq",

    "define": "Define",
    "enddef": "Enddef",
}

for (k,v) in directives.items():
    directives[k] = 'TokenType::Directive(Directive::%s)' % v

functions = {
    "abspath": "Abspath",
    "addprefix": "AddPrefix",
    "addsuffix": "AddSuffix",
    "and": "And",
    "basename": "BaseName",
    "call": "Call",
    "dir": "Dir",
    "error": "Error",
    "eval": "Eval",
    "file": "File",
    "filter": "Filter",
    "filterout": "FilterOut",
    "findstring": "FindString",
    "firstword": "FirstWord",
    "flavor": "Flavor",
    "if": "If",
    "info": "Info",
    "join": "Join",
    "lastword": "LastWord",
    "notdir": "NotDir",
    "or": "Or",
    "origin": "Origin",
    "patsubst": "PatSubst",
    "realpath": "Realpath",
    "sort": "Sort",
    "strip": "Strip",
    "subst": "Subst",
    "suffix": "Suffix",
    "value": "Value",
    "warning": "Warning",
    "wildcard": "Wildcard",
    "word": "Word",
    "wordlist": "WordList",
    "words": "Words",
}

for (k,v) in functions.items():
    functions[k] = 'TokenType::BuiltinFunction(BuiltinFunction::%s)' % v

# These are special because they don't need whitespace directly after them
custom_end_matcher = {
}


def build_rule_tree(d, init_rules):
    value_tree = init_rules
    if value_tree is None:
        value_tree = dict()

    for token in d.keys():
        parent_tree = value_tree;
        for c in token:
            if c not in parent_tree:
                parent_tree[c] = {}
            parent_tree = parent_tree[c]
        parent_tree['$'] = d[token]
    return value_tree

def collect_tokens(d):
    chars = set()
    for k in d.keys():
        for c in k:
            chars.add(c)
    return chars

# This defines a rust function that returns an Option<TokenType>,
# where the Some() variant represents matching a specific token type and a None
# represents a lack of a match. The paired usize is the end position of the
# last character consumed, which may be the same as the start if the matcher doesn't match
char_match_header = """
#[inline]
fn match{path}<IT> (last_end: usize, it: &mut Peekable<IT>) -> (usize, Option<TokenType>)
  where IT: Iterator<Item = (usize, char)>
{{
    if let Some((_, chr)) = it.peek() {{
        match chr.to_lowercase().next() {{"""

match_char_template = """
            Some('{char}') => {{
                let (start, chr) = it.next().unwrap();
                match{path}{path_char}(start + chr.len_utf8(), it)
            }}"""

match_whitespace_template = """
            Some(c) if !c.is_ascii_alphabetic() => {{
                (last_end, Some({token}))
            }}
            """

match_custom_template = """
            _ => {{
                {custom}(last_end, it);
            }}
"""

match_catchall_template = """
            _ => (last_end, None)"""

char_match_footer = """
        }}
    }} else {{
        (last_end, None)
    }}
}}
"""

char_match_terminal_footer = """
        }}
    }} else {{
        (last_end, Some({token}))
    }}
}}
"""

def gen_matcher(path, next_level):
    output = char_match_header.format(path=path)

    # Generate the match body
    for (k, v) in next_level.items():
        if k == '$':
            continue
        else:
            path_char = k
            if k == '-':
                path_char = 'dash_'
            output += match_char_template.format(path=path, path_char=path_char, char=k)

    if '$' in next_level:
        v = next_level['$']
        if v in custom_end_matcher:
            output += match_custom_template.format(custom=custom_end_matcher[v])
        else:
            output += match_whitespace_template.format(token=v)
            output += match_catchall_template
        output += char_match_terminal_footer.format(token=v)
    else:
        output += match_catchall_template
        output += char_match_footer.format()

    return output

# Get the name of the rule for the given output expression
def gen_final(output_expr):
    rule_name = output_expr[1]
    print('Generating output for rule', rule_name)

    return ''

def gen_rust_fns(rule_tree, path=None):
    output = ""

    if path is None:
        # This is the root, generate the special root function
        print('Doing root gen')
        output = "pub fn do_match<IT>(last_end: usize, first_char: char, it: &mut Peekable<IT>) -> (usize, Option<TokenType>)\n"
        output += "  where IT: Iterator<Item = (usize, char)>\n"
        output += "{\n"
        output += "    match first_char {\n"

        for (k, v) in rule_tree.items():
            assert k != '$'
            if k == '-':
                output += ' ' * 8 + "'-' => match_dash_(last_end, it),\n"
            else:
                output += ' ' * 8 + "'{char}' => match_{char}(last_end, it),\n".format(char=k)

        output += ' ' * 8 + "_ => (last_end, None)\n"

        output += "    }\n"
        output += "}\n"

        # And set up the initial path
        path = "_"

    for c in rule_tree.keys():
        new_path = path + c
        if c == '-':
            new_path = path + "dash_"
        if c == '$':
            print('Hit terminal for', rule_tree[c])
        else:
            output += gen_matcher(new_path, rule_tree[c])
            output += gen_rust_fns(rule_tree[c], new_path)
    return output

header_block = """
//! DO NOT EDIT THIS MODULE BY HAND.
//! It is automatically generated by the sibling python script, `{fname}`.

use super::*;
"""

def main():
    rule_tree = build_rule_tree(directives, None)
    rule_tree = build_rule_tree(functions, rule_tree)

    my_realpath = os.path.realpath(__file__)
    my_path = os.path.dirname(my_realpath)
    my_name = os.path.basename(my_realpath)
    output_path = os.path.join(my_path, "text_matcher.rs")

    with open(output_path, 'w') as outf:
        outf.write(header_block.format(fname=my_name))
        outf.write(gen_rust_fns(rule_tree))

if __name__ == '__main__':
    main()

# SPC-P-Conditional
partof: SPC-Parsing
###
We need to be able to parse several different conditionals:
This depends on [[SPC-variable-eval]].

## [[.ifdef]]
Determines if the variable defined by the rest of the line is
defined when the statement is encountered. Also
has a [[.ifndef]] variant.

## [[.ifeq]]
Determines if the expansion of the left and right sides
are defined when the statement is encountered.
Also has a [[.ifneq]] variant.

Must support the following formats:

[[.ifeq_parens]]
```make
ifeq (a,b)
endif
```

[[.ifeq_quotes]]
```make
ifeq 'a' "b"
endif
```
TODO: [[.expand]] Does MAKE expand things inside the quotes?
Note that the first set of quotes does not determine the format for the second set.

## [[.else]]
We need to recognize `else` in makefiles


# TST-P-Conditional
We need to test parsing of all the conditionals

Tests for [[SPC-P-Conditional.ifeq]]
 - [[.ifeq_parens]] Simple parenthesis case:
    `(a, b)` shall parse with LHS `a` and RHS `b`
 - [[.ifeq_nested_parens]] More complex parenthesis case:
    `($(foo_$(bar)), $(baz)_$(qux_$(big)))`
    should parse with LHS `$(foo_$(bar))`
    and RHS `$(baz)_$(qux_$(big))`
 - [[.ifeq_dquote]] `"a" "b"` should parse.
 - [[.ifeq_squote]] `'a' 'b'` should parse.
 - [[.ifeq_mixed_quote_ds]] `"a" 'b''` should parse.
 - [[.ifeq_mixed_quote_sd]] `'a' "b"'` should parse.
 - [[.ifeq_err_bad_token]] Anything not in the above formats should not parse
 
Tests for [[SPC-P-Conditional.ifdef]]
  - [[.ifdef_simple]] `ifdef a` should parse
  - [[.ifndef_simple]] `ifdef a` should parse
  - [[.ifdef_variable]] `ifdef $a` should parse
  - [[.ifdef_err_tokens]] `ifdef a b` should fail to parse,
    and report an error pointing to `b`

Tests for [[SPC-P-Conditional.else]]
  - [[.else_association]]
    The `else` block shall correctly identify its
    associated `if*` statement
  - [[.else_err_no_conditional]]
    The `else` directive shall error if it occurs outside of an open conditional.
  - [[.else_err_extra_tokens]]
    The `else` directive shall error if extra (non-comment) tokens occur
    after the else,
    with the exception of another conditional.
  - [[.else_next_conditional]]
    The `else` directive shall correctly recognize a follow-up conditional.
  - [[.else_comments_ok]]
    The `else` directive shall allow comments after it.
  - [[.else_err_too_many]]
    The `else` directive shall fail if there are too many elses
    on a given conditional. E.g.
    ```make
    ifdef foo
    else
    else # <- error here
    endif
    ```
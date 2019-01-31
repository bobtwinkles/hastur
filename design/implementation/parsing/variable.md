# SPC-P-V-Define
partof: SPC-P-Variable
###

TODO: flesh this out
  - Nested defines are allowed
  -


# SPC-P-V-Mod
partof:
- SPC-P-Directives
- SPC-P-Rule
- SPC-P-V-Define
- SPC-P-Variable
###
Some directives are valid anywhere a variable assignment is valid
and act as modifiers to those variable operations.
More precisely,
they are valid on target specific variables
as well as traditional variables.
As part of a variable assignment operation,
they shall terminate the currently active rule.
These are as follows

## [[.export]]
The `export` directives
control whether or not a variable is
exported to child processes.
Encountering one of these directives shall
terminate the currently active rule.
These directives shall operate on an expanded version
of the line,
to allow computed variable names.

## [[.override]]
The `override` directive
forces the variable to be set to
the specified value,
even if it was given a value on the command line.
Any subsequent assignment to a variable
that has been modified using the `override` modifier
shall have no effect unless that assignment also
uses the override directive.

## [[.private]]
Mark a variable as private.
This means it will not be inherited by prerequisites
for target-specific variables,
or will not be visible to any target
(for global variables).

## [[.undefine]]
The undefine modifier acts almost like a directive,
except that it can also be applied to target-specific
variables.
Does what it says on the tin.
No further modifiers shall be accepted after
encountering an undefine modifier.


# SPC-P-Variable
partof:
- REQ-Variable
- SPC-P-Rule
- SPC-Parsing
###
We need to accept the various types of variable assignment,
as well as their modifiers.

[[.terminates_rules]] All variable assignments shall
terminate the current rule.

## [[.simple]]
Simply expanded variables are given concrete values at the point of
their first assignment.
They are created by the `:=` or `::=` assignment operators.
For example, the following makefile will print `alpha beta`

```make
foo := alpha
foo_save := $(foo)
foo := beta

default:
    @echo $(foo_save) $(foo)
     
.PHONY: default
```

## [[.recursive]]
Recursively-expanded variable assignments are the ones created by
`=` assignment.
For example in the following makefile fragment,
`a` is a recursively expanded variable

```make
foo := foo
bar := bar

ref := foo

a = $($(ref))
a_sav := $a

default:
    @echo $(a_sav) $(a)

ref := bar

.PHONY: default

```
Recursively expanded variables
shall be evaluated at their point use.
Thus,
the above makefile
will print `foo bar`.
`a_sav` forces `$a` to expand immediately and captures the value,
since `a_sav` is simply expanded.
Because recipes are expanded during the execution phase,
`ref` will have the value `bar` during the expansion,
and thus `$a` will expand to `$($(ref))` and then to `$(bar)`.
Recursively expanded variables shall be stored as
[[SPC-variable-eval]] ASTs.

## [[.conditional_set]]
Make supports the `?=` operator
to conditionally set recursively-expanded variables.
The operator shall only have an effect if the variable being referenced 
does not already have a value.
That is, `a ?= $(foo)` will
only create a recursively-expanded variable with value `$(foo)`
if `a` has not been set previously.

## [[.append_set]]
Variables can be appended to using
the `+=`` assignment operator.
[[.append_set_fresh]]
If the variable was not previously defined,
a new recursively-expanded variable shall be created.
[[.append_set_recursive]]
If the variable was previously defined as recursively expanded,
the expression on the right hand side shall be added to the
expansion AST, prefixed by whitespace.
[[.append_set_simple]]
Otherwise, the variable must have been previously defined as
simply-expanded.
In this case,
a single space followed by the expansion of the right hand
side of the expression shall be appended to the existing value.

## [[.shell_set]]
TODO `!=`

## Implementation notes
This should be implemented as a recursive parser,
that first tries to recognize the directives
in [[SPC-P-V-Mod]] and then match against the assignment type.
TODO: flesh this out [[.impl]]


# TST-P-V-Mod
The majority of tests shall have
both global and target-local variable
variants.
The target-local variants shall be prefixed with `tgt_`

Tests for the [[.export]] modifier:
 - [[.export_straightforward]] The directive shall
   work in the simple case of `export a := b`
 - [[.export_TODO]]


# TST-P-Variable
[[.terminates_rules]] Variable assignment of any sort shall
terminate the currently active rule.
Tests prefixed with `db_` shall test
the impact of each variable on the database.

Tests for [[.simple]] variable expansions:
  - [[.simple_assignment]] A single `a := b` statement shall
    produce a simply-expanded variable
  - [[.simple_nospace]] An assignment with no space between the variable name
    and the assignment operator
  - [[.simple_constructed]] All variable references shall
    be expanded in `a := $(foo_$(bar))`
  - [[.simple_dcolon]] An assignment of the form `a ::= b` shall
    create a simply-expanded variable
  - [[.dcolon_nospace]] A POSIX assignment with no space between the variable name
    and the assignment operator
  - [[.db_simple]] A simple variable expansion shall
    add the literal value to the database.
  
Tests for [[.recursive]]
  - [[.recursive_assignment]] A single `a = b` statement 
    shall produce a recursively-expanded variable
  - [[.recursive_nospace]] Recursive assignment with no space between the
    variable name and the operator
  - [[.recursive_defer]] The statement `a = $(foo)` shall
    produce a recursively-expanded variable with
    an appropriate variable evaluation AST.
  - [[.db_recursive]] A recursively expanded variable
    shall be added to the database.

Tests for [[.conditional_set]]
  - [[.conditional_assignment]] A single `a ?= b` statement shall
    produce a conditionally set recursively expanded variables
  - [[.conditional_expansion]] The statement `a ?= $(foo)` shall
    produce an expansion AST.
  - [[.db_conditional]] The database shall modify the value of
    a variable from conditional assignment
    only if that variable is not yet set inf the database.

Tests for [[.append_set]]
  - [[.append_parse]] An append statement `a += $(b)`
    shall parse to the expected AST.
  - [[.db_append_none]] An append statement to
    a previously non-extant variable shall
    result in the creation of a new recursively-expanded variable.
  - [[.db_append_rec]] An append statement to
    a previously defined recursively expanded variable shall result in
    the AST of the RHS along with a leading space being appended to
    the variable's AST.
  - [[.db_append_simple]] An append statement to
    a previously defined simply-expanded variable shall
    result in the evaluation of the RHS preceded by a space
    being appended to the previous value.
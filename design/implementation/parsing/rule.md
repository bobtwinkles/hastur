# SPC-P-Rule
partof: SPC-Parsing
###

When parsing rules,
we need to identify the three components
of the rule.
Rule lines shall be parsed after first attempting
to parse the line as a [[SPC-P-Variable]] line.
The line itself is expanded only incrementally 
(i.e. only one whitespace-separated group of tokens 
at a time).
After every expansion step,
we first check to see if a colon has
been found.
This is expansion-and-check dance is
somewhat complex,
and is explained in [[.line_expansion]].
After we find a colon,
we collect the list of targets by
using [[SPC-File-List]].

TODO: describe the rest of parsing

## [[.line_expansion]]
Line expansion is complex for several reasons
 - We need to figure out if
    we have target-specific variables before
    expanding the whole line.
    Such variable may be recursive
    and thus we want to store their literal values,
    rather than what they expand to at this time.
 - We can't just stop at the first `:` since
    that colon may be part of a drive name when
    [[.target_path_windows]] is in effect.
    We detect drive-spec paths by
    attempting to find a
    drive specifier letter,
    followed by a colon and backslash
    at the beginning of a target.

An example might help clarify things

```make
a := foo

targ1: c := $a
targ1: d = $a

a := bar

targ1:
    @echo $a $c $d
```
This will print `bar foo bar`.
See [[SPC-variable-eval]] and [[SPC-P-Variable]] for
more details on
variable assignment and
evaluation.


# SPC-P-Target-List
partof: SPC-P-Rule
###
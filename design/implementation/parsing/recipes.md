# SPC-P-Recipe
partof: SPC-Parsing
###
A recipe is anything that starts with the
current command character after the first
target line.
In some ways recipe lines are the simplest to
parse:
if we didn't care about their structure,
we would simply read the string into a buffer.
Unfortunately for us,
we do care about the structure.
We want to parse out their structure
to the greatest extent possible during the parsing phase.
See [[SPC-S-Recipe]] for how we do sensitivity analysis of recipe lines.
Recipe lines shall not be evaluated
when encountered.
Instead,
they are evaluated only after the whole
Makefile has been read in.
Thus, recipe lines as output from the parsing stage
shall store an AST from the [[SPC-variable-eval]] engine
suitable for later evaluation.


# TST-P-Recipe

[[.TODO]]
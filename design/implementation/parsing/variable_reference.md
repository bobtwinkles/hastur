# SPC-P-V-Ref
partof:
- SPC-P-Variable
- SPC-Parsing
###
Variable references are introduced with a 
`$` character.
[[.constant_dollar]] If the next character is
a dollar sign or the end of the line,
then the value of the expression is just a constant dollar sign.
[[.single_char]] If the next character
is a non-space character,
and not '(' or '(',
treat it as a reference to a variable
with a single-character name.

## [[.complex]]
If the reference starts with a `(` or `{`,
we need to parse out the full AST.
There shall be a single function
which shall consume a makefile line
and produce an AST.
This means recognizing all Make functions,
as well as substitution reference syntax.
These are listed below:

 - [[.abspath]]
  Once the 
 - [[.addprefix]]
 - [[.addsuffix]]
 - [[.and]]
 - [[.basename]]
 - [[.call]]
 - [[.dir]]
 - [[.error]]
 - [[.eval]]
 - [[.file]]
 - [[.filter]]
 - [[.filter_out]]
 - [[.findstring]]
 - [[.firstword]]
 - [[.flavor]]
 - [[.foreach]]
 - [[.if]]
 - [[.info]]
 - [[.join]]
 - [[.lastword]]
 - [[.notdir]]
 - [[.or]]
 - [[.origin]]
 - [[.patsubst]]
 - [[.realpath]]
 - [[.shell]]
 - [[.sort]]
 - [[.strip]]
 - [[.subst]]
 - [[.substitution_reference]]
 - [[.suffix]]
 - [[.value]]
 - [[.warning]]
 - [[.wildcard]]
 - [[.word]]
 - [[.wordlist]]
 - [[.words]]
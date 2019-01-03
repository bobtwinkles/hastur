# SPC-Parsing
partof: REQ-interface
###
Parsing should be implemented using
the `nom` parser-combintor library.

There are many parts of a Makefile we need to parse accurately.
Each of these parsers is applied one at a time in order until
one succeeds:
 - [[.bom]] Test and skip the UTF-8 BOM
 - [[SPC-P-Recipe]]: Recipe lines
 - [[SPC-P-Variable]]: Variable assignments
 - [[SPC-P-Conditional]]: Conditional statements
 - [[.ignoring_check]]:
   TODO: Figure out how this plays into [[SPC-Sensitivity]] [[.ignoring_sensitivity]]
   At this point, we decide if we're going to ignore the line.
   The `ignoring` flag is set when we're in the false side of a `if*` directive
 - [[SPC-P-Directives]]: Various directives
 - [[.final_command_check]]:
   If something slipped through to here
   (i.e. was not matched) by anything above,
   but the line starts with the command character,
   we need to error out at this stage
   ("recipe commences before first target").
 - [[SPC-P-Rule]]:
   Finally, we can conclude that this line
   has something to do with target files.
   It may not be a full
   `target: deps` line,
   since target local variables exist,
   but it's definitely going to have something to do with
   targets.
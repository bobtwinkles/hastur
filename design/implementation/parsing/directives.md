# SPC-P-Directives
partof: SPC-parsing
###
We should at least understand all directives GNU Make supports,
even if we don't act on them.
I'm looking at you `load`.

## [[.export]]
The `export` and `unexport` directives control whether or not global variables are
exported to child processes via the environment.
By default,
most variables are not exported.
Variable expansion shall be performed on
the full line before further processing of these directive,
to support constructed variable names.
If given no arguments,
both directives shall operate on all global variables.

## [[.vpath]]
The `vpath` directive controls the virtual
search path for source files.
See [[SPC-Paths]] for how this interacts
with the rest of the system.
Encountering this directive shall
terminate the currently active rule.
This directive shall operate on a
variable-expanded line.
The directive has three modes of operation
  - [[.vpath_clear]]
    `vpath` shall clear all known vpaths.
  - [[.vpath_clear_specific]]
    `vpath pattern` shall clear all vpaths associated with `pattern`
  - [[.vpath_specify]]
    `vpath pattern directories`
    Set the vpath associated with all files matching `pattern` to
    `directories`.
    On non-Windows platforms, `directories` is `:` separated.
    It is `;` separated on Windows.

## [[.include]]
The `include`, `-include`, and `sinclude`
directives pull an existing makefile into this one.
In a normal Make implementation,
attempting to `include`
a non-extant file will result in the parser
continuing without errors
with that target included in the list of targets to make.
It's not clear what we should do in that situation.

These directives shall operate on a line that is
variable-expanded.
These directives shall terminate the currently waiting rule.
These directives shall not influence the conditional state of the
parent Makefile.
These directives shall use [[SPC-File-List]] to determine the list of makefiles to include.

## [[.load]]
We shall recognize the `load` and `-load` directives.
Encountering it shall produce an error,
there is no reasonable way for us to support it.
See [the GNU Make manual](https://www.gnu.org/software/make/manual/make.html#Loading-Objects)
if you really want the gory details.


# TST-S-Directives
partof: SPC-P-Directives
###

Tests for the `export` directives
 - [[.export_simple]] `export a` shall mark the variable `a` as exported
 - [[.unexport_simple]] `unexport a` shall mark the variable `a` as unexported
 - [[.export_multi]] `export a b` shall mark both `a` and `b` as exported
 - [[.export_resolve]] `export $a` shall mark `b` as exported when `a := b`.
 
Tests for the `vpath` directives
 - [[.vpath_clear]] `vpath` shall clear all known vpaths
 - [[.vpath_pattern_clear]] `vpath pattern`
   shall clear only the paths associated with `pattern`.
 - [[.vpath_simple]] `vpath pattern directory` shall
   shall add `directory` to the search path for files
   in `pattern`.
 - [[.vpath_unix_sep]] In UNIX compatibility mode,
   the directory parameter of a `vpath` shall be separated
   by `:`.
 - [[.vpath_windows_sep]] In Windows compatibility mode,
   the directory parameter of a `vpath` shall be separated
   by `;`.
 - [[.vpath_expand]] `vpath` shall expand the line

Tests for the `include` directive
 - [[.include_simple]] `include` should add the contents
   of an existing file to the database,
   without disturbing conditional state.
 - [[.sinclude_simple]] `sinclude` shall continue without
   error if the file doesn't exist.
 - [[.dash_include_simple]] `-include` shall continue without
   error if the file doesn't exist.
 - [[.include_glob]] `include` shall glob appropriately.
 - [[.include_expand]] `include` shall expand the line

Tests for the `load` directive
  - [[.load_err]] Encountering the `load` or `-load` directive
    shall produce an error
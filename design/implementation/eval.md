# SPC-variable-eval
partof:
- REQ-interface
- REQ-Variable
- SPC-P-Conditional
- SPC-P-V-Mod
- SPC-P-Variable
- SPC-Parsing
###

The library shall be able to evaluate the AST
specified by [[SPC-V-AST]].
Evaluation shall return 
Details on evaluation are outlined below:

## [[.constant]]
A constant string.
Simply copied into the output buffer.

## [[.concat]]
This AST node maintains a list of children,
who are to be evaluated and then concatenated in order

## [[.variable_reference]]
This node has a single AST node child,
representing the name of the variable to dereference

## [[.substitution_reference]]
Has 3 AST node children:
the variable name to dereference,
the substitution pattern,
and the replacement pattern.

## Functions for string substitution and analysis
These are all the functions from
[section 8.2 in the GNU make manual](https://www.gnu.org/software/make/manual/make.html#Text-Functions)

  - [[.subst]]
    `$(subst from,to,text)`

    Substitutes `from` with `to` in `text`.
    Allows only precise matching.
  - [[.patsubst]]
    This function should not be given its own AST node,
    since it is equivalent to a substitution reference
    that includes a `%` character.
  - [[.strip]]
    Represents the make `strip` function.
  - [[.findstring]]
    `$(findstring find,in)`
    
    This function returns the match if `find` is found in `in`,
    or the empty string otherwise.
  - [[.filter]]
    `$(filter pattern1 pattern2 ... patternN,text)`
    
    Returns the whitespace-separated words in `text` which match
    any of the patterns
  - [[.filter_out]]
    `$(filter-out pattern1 pattern2 ... patternN,text)`

    Just like `filter`,
    but returns all the words that do not match any of the patterns
  - [[.sort]]
    `$(sort list)`
    
    Sorts all the whitespace-separated words in `list`
  - [[.word]]
    `$(word n,text)`
    
    Returns the `n`th whitespace-separated word in `text`.
    These lists are 1-indexed.
  - [[.wordlist]]
    `$(wordlist s,e,text)`
    
    Returns the whitespace separated words in wordlist
    in `[s, e]`
    
  - [[.words]]
    `$(words text)`
    
    Returns the number of whitespace-separated words in `text`
    
  - [[.firstword]]
    `$(firstword text)`
    
    Returns the first whitespace-separated word in `text`
    
  - [[.lastword]]
    `$(lastword text)`
    
    Returns the last whitespace-separated word in `text`
    
## Functions for file names
All the functions form
[section 8.3 of the GNU Make manual](https://www.gnu.org/software/make/manual/make.html#File-Name-Functions)

  - [[.dir]]
    `$(dir name1 name2 ... nameN)`
    
    Returns a list of strings representing the
    directory names of all the file names in the list.
  - [[.notdir]]
    `$(dir name1 name2 ... nameN)`
    
    Returns the non-directory components of the file names.
  - [[.suffix]]
    `$(suffix name1 name2 ... nameN)`
    
    Returns the part of the notdir components of the file name
    after the last `.` in that part.
  - [[.basename]]
    `$(basename name1 name2 ... nameN)`
    
    Extracts everything but what the `suffix` function would return
  - [[.addsuffix]]
    `$(addsuffix suffix,name1 name2 ... nameN)`

    Adds a suffix to every file in the list of names
  - [[.addprefix]]
    `$(addprefix suffix,name1 name2 ... nameN)`

    Adds a prefix to every file in the list of names
    
  - [[.join]]
    `$(join list1,list2)`
    
    Zips the two space-separated lists together
  - [[.wildcard]]
    `$(wildcard pattern)`
    
    Searches for all the files matching a given wildcard pattern.
    See [[SPC-File-List]] for details on wildcarding.
    Also check the implementation of `function.c:string_glob`
    
  - [[.realpath]]
    `$(realpath file1 file2 ... fileN)`
    
    Gets the canonical absolute path for each file.
    `realpath(3)` specifies precise behavior.
  - [[.abspath]]
    `$(abspath file1 file2 ... fileN)`
    
    Cleans up file names approximately,
    rather than by using a proper `realpath` implementation.
    Removes duplicate path separators,
    `./` components,
    and `../` components.

## Functions for conditionals
All the functions from
[Section 8.4 of the GNU make manual](https://www.gnu.org/software/make/manual/make.html#Conditional-Functions)

  - [[.if]]
    `$(if condition,then-part[,else-part])`
    
    If `condition` evaluates to something non-empty,
    the expression evaluates to `then-part`.
    Otherwise it evaluates to the `else-part`,
    which may be an empty string
  - [[.or]]
    `$(or condition1,condition2,...,conditionN)`

    Evaluates each of the conditions until
    one of them expands to something non-empty.
    The value of that condition is then the value of the expression
  - [[.and]]
    `$(and condition1,condition2,...,conditionN)`
    
    Expands each condition,
    returning the value of `conditionN` if all
    prior conditions expanded to something non-empty.
    Otherwise,
    evaluates to the empty string

## Misc. functions
The rest of the functions from
[section 8](https://www.gnu.org/software/make/manual/make.html#toc-Functions-for-Transforming-Text)

  - [[.foreach]]
    `$(foreach var,list,text)`
    
    Evaluates `text` multiple times,
    modifying the variable context each time
    such that `var` is set to each of the
    space-separated words in `list`
    
  - [[.file]]
    `$(file op filename[,text])`

    The space between `op` and
    the filename
    is optional.
    `op` must be one of the following.
    - `>`: `text` overwrites the contents of `filename`.
      The file must always end with a line break
    - `>>`: `text` is appended to the contents of `filename`.
      The file must always end with a line break
    - `<`: The contents of `filename` are read in,
      and returned as the result of the expression.
      Providing `text` with this operation is an error.
  - [[.call]]
    `$(call variable,param1,param2,...,paramN)`
    
    Expands `variable` with `1 := param1`, `2 := param2`, etc.
    Note that all the parameters are expanded prior
    to expanding `variable` in context.
  - [[.value]]
    `$(value variable)`
    
    Returns the literal contents of a variable,
    without expansion (even for recursive variables).
  - [[.eval]]
    `$(eval text)`

    Evaluates `text` as if it were in an included makefile.
    
  - [[.origin]]
    `$(origin variable)`
    
    Tells you what sort of assignment
    last impacted the value of `variable`.
    - `undefined` if the variable isn't defined at all.
    - `default` if it is part of the set of variables defined by Make itself.
      If the user has overwritten a default variable,
      the origin of that overwrite shall be returned instead.
    - `environment` if the variable came from the environment
    - `environment override` if the variable came from the environment,
      and is shadowing a in-file definition because of `-e`.
    - `file` if the variable was set somewhere in the makefile
    - `command line` if the variable was set on the command line
    - `override` if the variable was defined with an override directive
    - `automatic` if the variable is one of the special ones
      defined for recipe evaluation.
  - [[.flavor]]
    `$(flavor variable)`
    
    Tells you something about how `variable` is to be expanded
    - `undefined` if the variable is not defined
    - `recursive` if the variable is recursive
    - `simple` if the variable is simple
  - [[.error]]
    `$(error text)`
    
    Stops makefile processing and emits `text` (after expansion)
    as an error message
  - [[.warning]]
    `$(warning text)`
    
    Emits a warning message with the expansion of `text`.
  - [[.info]]
    `$(info text)`
    
    Emits an info-level message with the expansion of `text`.
    
  - [[.shell]]
    `$(shell op)`
    
    Evaluates `op` using the Make shell.
    Standard output is captured and returned as the value of the expression.
    [[.shell_debug]] We should capture stderr as well,
    and surface that somehow.
    [[.shellstatus]] The `.SHELLSTATUS` variable should be updated.
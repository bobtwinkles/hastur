# SPC-V-AST
partof:
- SPC-P-V-Ref
- SPC-Variable-Eval
###
This outlines the different AST nodes required to
represent variable expansion expressions.
We will use the following expression as an example:

```
$(foo_$(bar:.c=.o)_$(strip $(baz)))
```
Which produces the AST

```
VariableReference {
    Concat {
        Constant("foo_"),
        SubstitutionReference(
            Constant("bar"),
            Constant(".c"),
            Constant(".o")
        ),
        Constant("_"),
        Strip(
            VariableReference(Constant("baz"))
        )
    }
}
```

The AST shall support the following nodes:

 - [[.constant]] for constant strings
   (strings listed literally in the makefile)
 - [[.concat]] for when a bunch of
   other AST nodes need to be concatenated
   to form the full value
 - [[.variable_reference]] for when we need
   to dynamically dereference a variable

We also need nodes for the following Make functions.
 - [[.abspath]] for the `abspath` function.
   This node shall have a single AST node child,
   representing the list of files.
 - [[.addprefix]] for the `addprefix` function.
   This node shall have two AST node children,
   one representing the prefix and
   the other representing the list of things to add the prefix too.
 - [[.addsuffix]] for the `addsuffix` function.
   This node shall have two AST node children,
   one representing the suffix and
   the other representing the list of things to add the suffix too.
 - [[.and]] for the `and` function.
   This node shall have a vector of AST node children,
   representing each of the cases.
 - [[.basename]] for the `basename` function.
   This node shall have a single AST node child,
   representing the list of things that should have their
   basename extracted.
 - [[.call]] for the `call` function.
   This node shall have a vector of children.
   The first child shall be the name of the variable to be expanded.
   The reset of the children shall be the parameters to the call.
 - [[.dir]] for the `dir` function.
   This node shall have a single AST node child,
   representing the list of things on which to apply the `dir` function.
 - [[.error]] for the `error` function.
   This node shall have a single AST node child,
   representing the text of the error message.
 - [[.eval]] for the `eval` function.
   This node shall have a single AST node child,
   representing the text to be evaluated.
 - [[.file]] for the `file` function.
   This node shall have a `mode` enum representing the operation
   to be performed.
   This enum shall have the following variants:
   - [[.file_write]] for the variant representing the
     overwrite operation. It shall have a single AST node child,
     representing the text to write.
   - [[.file_append]] for the variant representing the
     append operation. It shall have a single AST node child,
     representing the text to append.
   - [[.file_read]] for the variant representing
     a read operation.
 - [[.filter]] for the `filter` function.
   This node shall have two children,
   the first representing the list of filters
   and the second representing the list to be filtered.
 - [[.filter_out]] for the `filter-out` function.
   This node shall have two children,
   the first representing the list of filters
   and the second representing the list to be filtered.
 - [[.findstring]] for the `findstring` function.
   This node shall have two children,
   the first representing the needle
   and the second representing the haystack.
 - [[.firstword]] for the `firstword` function.
   This node shall have one child,
   representing the list to be retrieved from.
 - [[.flavor]] for the `flavor` function.
   This node shall have one child,
   representing the name of the variable to be queried.
 - [[.foreach]] for the `foreach` function.
   This node shall have three children.
   One representing the variable name,
   one representing the list of values,
   and one representing the thing to be expanded.
 - [[.if]] for the `if` function.
   This node shall have three children.
   One representing the conditional,
   one representing the then-clause,
   and one representing the else-clause.
   The else-clause may be `Constant("")`
   if it does not exist in the source makefile.
 - [[.info]] for the `info` function.
 - [[.join]] for the `join` function.
 - [[.lastword]] for the `lastword` function.
 - [[.notdir]] for the `notdir` function.
 - [[.or]] for the `or` function.
 - [[.origin]] for the `origin` function.
 - [[.patsubst]] for the `patsubst` function.
 - [[.realpath]] for the `realpath` function.
 - [[.shell]] for the `shell` function.
 - [[.sort]] for the `sort` function.
 - [[.strip]] for the `strip` function.
 - [[.subst]] for the `subst` function.
 - [[.suffix]] for the `suffix` function.
 - [[.value]] for the `value` function.
 - [[.warning]] for the `warning` function.
 - [[.wildcard]] for the `wildcard` function.
 - [[.word]] for the `word` function.
 - [[.wordlist]] for the `wordlist` function.
 - [[.words]] for the `words` function.
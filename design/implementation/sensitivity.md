# SPC-Sensitivity
partof: REQ-Interface
###
In order to efficiently reparse Makefiles,
we need to track what parts of the makefile (i.e. what rules and what variables)
are sensitive to which variables.

To do this,
all functions that reference the variable environment shall return
a `SensitivitySet` which is the union of all variables names referenced.
We maintain two types of sensitivity:
global and local.
Global sensitivity results from things like
`ifdef` directives,
and the expansion leading to the text inside an `$(eval ...)`.
Local sensitivity is relevant only to recipes,
and represents the set of variables
that determine the final command line that gets executed.
As an example, consider the Makefile below

```make
a_bin : a.c
ifdef USE_CC
  $(CC) -o $@ $<
else
  $(CPP) -o $@ $<
endif
```
The third line (`$(CC) -o $@ $<`) is
globally sensitive to `USE_CC` and locally
sensitive to `CC` and
the special variables `@` and `<`.

[[.BIG_NOTE]]
there is some nightmare inducing interplay between vpaths and
anything that changes its results based on a file search (glob...)
I need to run some experiments to determine if e.g. `glob`
is sensitive to `vpath`.
It's tempting to say that changing `vpath` forces a full reparse.
That shouldn't be *too* expensive.
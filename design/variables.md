# REQ-Variable
partof:
- REQ-interface
- REQ-purpose
###
Variables are the heart and soul of Makefiles.
Their interactions are varied, complex, and subtle.

## [[.priority]]
Make has interesting rules regarding the priority of variable assignments,
especially in the context of the `-e` switch.
From high to low,
the priorities are
  - target-specific variables assigned with the `override` qualifier.
  - target-specific variables
  - global variables assigned with the `override` qualifier
  - environment variables brought in with `-e`.
  - global variables
  - environment variables
  
## [[.target_specific]]
Target-specific variables are relatively straightforward:
when executing the recipe for a target,
they provide some overrides to the global variable context.
Target-specific variables are inherited by prerequisites
([[.target_specific_inherit]]),
so in the following makefile

```
a: CFLAGS += -g

a: b.o
  $(CC) $(CFLAGS) -o $@ $<
  
b.o: b.c
  $(CC) $(CFLAGS) -c -o $@ $<
```

`b.o` will be built with `CFLAGS` containing `-g`.

## [[.private]]
Private variables are not inherited into
any context.
For global variables,
this means that they are not visible to recipes
for any target.
For target-specific variables this means
that they are not inherited by prerequisite
any context.
For global variables,
this means that they are not visible to recipes
for any target.
For target-specific variables this means
they are not inherited by prerequisites.
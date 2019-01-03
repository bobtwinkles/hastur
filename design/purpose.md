# REQ-interface
partof: REQ-purpose
###

The library should follow a philosophy of
making easy things easy,
and hard things possible.
For example,
we consider the simple case below.
More complex cases are explored in the children of this
specification.

Given the following Rust code,

```rust
use hastur::Makefile;

fn main() {
    let makefile = Makefile::read_from_file("test.mk")
    for rule in makefile.rules() {
        println!("-- Rule --");
        println!("targets: {:?}", rule.targets);
        println!("dependencies: {:?}", rule.dependencies);
        println!("recipe:\n{:?}", rule.recipe());
    }
    
    println!("\nUpdating variable\n");
    makefile.set_variable("CC", "cheese-compiler");

    for rule in makefile.rules() {
        println!("-- Rule --");
        println!("targets:", rule.targets);
        for target in rule.targets {
            println!("{:?}", target.flatten());
        }
        println!("deps:", rule.dependencies);
        for dep in rule.dependencies {
            println!("{:?}", dep.flatten());
        }
        println!("recipe:\n{:?}", rule.recipe().flatten());
    }
}

```

Assuming it was provided with the following test file
```make
ifeq ($(CC),cheese-compiler)
epic_binary : a.c
else
epic_binary : b.c
endif
        $(CC) -o $@ $<
        
regular_binary: c.c
        $(CC) -o $@ $<
```

the above program shall produce the following output
```
-- Rule --

** NOTE: we have strings in the "contents" and "location" fields, but these
should actually be string intern symbols and Location structures respectively **

targets: [ Evaluated::Constant(LocatedSpan { location: "test.mk:4:1", contents: "epic_binary"}) ]
dependencies: [ Evaluated::Constant(LocatedSpan { location: "test.mk:4:9", contents: "a.c"}) ]
recipe:
[
  Evaluated::Concat [
    Evaluated::VariableExpansion {
      name: Evaluated::Constant(LocatedSpan { location: "test.mk:6:4", contents: "CC" }),
      value: Evaluated::Constant(LocatedSpan { location: "<<internal>>:0:0", contents: "cc" })
    },
    Evaluated::Constant(LocatedSpan { location: "test.mk:6:7", content " -o " }),
    Evaluated::VariableExpansion {
      name: Evaluated::Constant(LocatedSpan { location: "test.mk:6:8"), contents: "@"},
      value: Evaluated::Constant(LocatedSpan { location: "test.mk:4:1"), content: "epic_binary"},
    },
    Evaluated::Constant(LocatedSpan { location: "test.mk:6:13", content: " "}),
    Evaluated::VariableExpansion {
      name: Evaluated::Constant(LocatedSpan { location: "test.mk:6:14", contents: "<"}),
      value: Evaluated::Constant(Locatedspan { location: "test.mk:4:15", content: "b.c" }),
    }
  ]
]
-- Rule --
** SNIP **
Similar to above, but for the second rule
** SNIP **


Updating variable

-- Rule --
targets:
"epic_binary"
dependencies:
"b.c"
recipe:
cheese-compiler -o epic_binary b.c
-- Rule --
targets:
"regular_binary"
dependencies:
"c.c"
recipe:
cheese-compiler -o regular_binary c.c
```

The library should accelerate the computation of
the second set of rules
by only reparsing the parts of the makefile that
can actually be affected by the changed variable.

For more detailed interface descriptions,
consider checking out the child artifacts:
 - [[REQ-parse-control]]: Control of the parser up-front
 - [[REQ-expansion-tracking]]: Tracking of the expansion system


# REQ-purpose
The goal of this project is
to provide a library
suitable for
interactive exploration and
understanding of
makefiles.
To this end,
we aim to provide
a simple interface for parsing makefiles,
and then updating assumptions about
the environment in which that makefile executes.
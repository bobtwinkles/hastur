# REQ-Expansion-Tracking
partof: REQ-interface
###
It shall be possible for the user to determine
where each component of text came from and
the path taken to get there.
We will use a fairly involved example

```make
define mkrule =
$(1): $(2:.c=.o)
	@echo $$@
	@echo ----
	@echo $$^
endef

$(eval $(call mkrule,default,a.c b.c))

.PHONY: default a.o b.o
```
The `eval` statement on line 8 will produce the following structure:
```
Evaluated::Eval(
  source: Evaluated::Call(
    arguments: [
      Evaluated::Constant { LocatedSpan { location: "test.mk:8:15", contents: "mkrule"} },
      Evaluated::Constant { LocatedSpan { location: "test.mk:8:22", contents: "default"} },
      Evaluated::Constant { LocatedSpan { location: "test.mk:8:30", contentS: "a.c b.c"} },
    ],
    result: Evaluated::Concat {[
      Evaluated::VariableExpansion {
        name: Evaluated::Constant(LocatedSpan { location: "test.mk:2:3", contents: "1"}),
        value: Evaluated::Constant(LocatedSpan { location: "test.mk:8:22", contents: "default"}),
      },
      Evaluated::Constant { LocatedSpan { location: "test.mk:2:5", contents: ": "}},
      Evaluated::SubstitutionExpansion {
        name: Evaluated::Constant(LocatedSpan { location: "test.mk:2:9", contents: "2"}),
        pattern: Evaluated::Constant(LocatedSpan { location: "test.mk:2:11", contents: ".c"}),
        replacement: Evaluated::Constant(LocatedSpan { location: "test.mk:2:14", contents: ".o" }),
        value: Evaluated::Concat {[
          Evaluated::Constant(LocatedSpan { location: "test.mk:8:30", contents: "a"}),
          Evaluated::Constant(LocatedSpan { location: "test.mk:2:14", contents: ".o" }),
          Evaluated::Constant(LocatedSpan { location: "test.mk:8:33", contents: " b"}),
          Evaluated::Constant(LocatedSpan { location: "test.mk:2:14", contents: ".o" }),
          Evaluated::Constant(LocatedSpan { location: "test.mk:2:16", contents: "\n"}),
        ]},
      },
      Evaluated::Constant(LocatedSpan { location: "test.mk:3:1", contents: "\t@echo "}),
      Evaluated::Constant(LocatedSpan { location: "test.mk:3:10", contents: "$"}),
      Evaluated::Constant(LocatedSpan { location: "test.mk:3:11", contents: "@\n\t@echo ----\n\t@echo "}),
      Evaluated::Constant(LocatedSpan { location: "test.mk:6:10", contents: "$"}),
      Evaluated::Constant(LocatedSpan { locatino: "test.mk:6:11", contents: "^"}),
    ]}
  )
)
```

Flattened, this is the makefile fragment
```
default: a.o b.o
    @echo $@
    @echo ----
    @echo $^
```

Then the rule produced will be structured as follows

```
MakefileRule {
  targets: [
    Evaluated::Eval(
      source: Evaluated::Call(
        arguments: [
          Evaluated::Constant { LocatedSpan { location: "test.mk:8:15", contents: "mkrule"} },
          Evaluated::Constant { LocatedSpan { location: "test.mk:8:22", contents: "default"} },
          Evaluated::Constant { LocatedSpan { location: "test.mk:8:30", contentS: "a.c b.c"} },
        ],
        result: Evaluated::Concat {[
          Evaluated::VariableExpansion {
            name: Evaluated::Constant(LocatedSpan { location: "test.mk:2:3", contents: "1"}),
            value: Evaluated::Constant(LocatedSpan { location: "test.mk:8:22", contents: "default"}),
          },
          Evaluated::Constant { LocatedSpan { location: "test.mk:2:5", contents: ": "}},
          Evaluated::SubstitutionExpansion {
            name: Evaluated::Constant(LocatedSpan { location: "test.mk:2:9", contents: "2"}),
            pattern: Evaluated::Constant(LocatedSpan { location: "test.mk:2:11", contents: ".c"}),
            replacement: Evaluated::Constant(LocatedSpan { location: "test.mk:2:14", contents: ".o" }),
            value: Evaluated::Concat {[
              Evaluated::Constant(LocatedSpan { location: "test.mk:8:30", contents: "a"}),
              Evaluated::Constant(LocatedSpan { location: "test.mk:2:14", contents: ".o" }),
              Evaluated::Constant(LocatedSpan { location: "test.mk:8:33", contents: " b"}),
              Evaluated::Constant(LocatedSpan { location: "test.mk:2:14", contents: ".o" }),
              Evaluated::Constant(LocatedSpan { location: "test.mk:2:16", contents: "\n"}),
            ]},
          },
          Evaluated::Constant(LocatedSpan { location: "test.mk:3:1", contents: "\t@echo "}),
          Evaluated::Constant(LocatedSpan { location: "test.mk:3:10", contents: "$"}),
          Evaluated::Constant(LocatedSpan { location: "test.mk:3:11", contents: "@\n\t@echo ----\n\t@echo$"}),
          Evaluated::Constant(LocatedSpan { location: "test.mk:6:10", contents: "$"}),
          Evaluated::Constant(LocatedSpan { locatino: "test.mk:6:11", contents: "^"}),
        ]}
      )
    )
  ]
  deps: [
  ]
  recipe: [
  ]
}
LocatedString {
    contents: [
        LocatedSpan { location: "test.mk:3:1", content: "@echo ", expanded_at: Some(
            LocatedString { contents: [ LocatedSpan { 
                location: "test.mk:8:10",
                content: "call mkrule,default,a.c b.c",
                expanded_at: Some(
                    LocatedString { contents: [ LocatedSpan { 
                        location: "test.mk:8:10",
                        content: "exec $(call mkrule,default,a.c b.c)",
                        expanded_at: None
                    }]}
                )
            }]}
        )},
        LocatedSpan { location: "test.mk:8:22", content: "default", expanded_at: Some(
            LocatedString { contents: [ LocatedSpan {
                LocatedString { contents: [ LocatedSpan { 
                    location: "test.mk:8:10",
                    content: "call mkrule,default,a.c b.c",
                    expanded_at: Some(
                        LocatedString { contents: [ LocatedSpan { 
                            location: "test.mk:8:10",
                            content: "exec $(call mkrule,default,a.c b.c)",
                            expanded_at: None
                        }]}
                    )
                }]}
            }]}
        )}
    ]
}
```
# SPC-S-Conditional
partof: SPC-Sensitivity
###
Here we define how sensitivity is impacted by conditionals.

## [[.ifdef]] and [[.ifeq]]
`ifdef`, `ifeq` (and their `ifn*` variants)
produces global sensitivity to
the variable we are testing against,
along with all the variables expanded to
construct the variable names.

## [[.else]]
`else` statements don't change sensitivity,
unless they are paired with an additional conditional,
in which case they add that conditional's sensitivity
to the global sensitivity set.

## [[.endif]]
`endif` directives terminate their matching 
`if*` directive's sensitivity,
including the sensitivity of any parent directives
chained through `else` directives.


# TST-S-Conditional
Tests for conditional sensitivity

Tests for [[SPC-S-Conditional.ifeq]]
 - [[.ifeq_simple]] 
   After a statement
     `ifeq ($(foor), $(bar))`
   The global sensitivity list should contain `foo` and `bar`

Tests for [[SPC-S-Conditional.ifdef]]
 - [[.ifdef_simple]] 
   After a statement
     `ifdef ($(foor), $(bar))`
   The global sensitivity list should contain `foo` and `bar`

Tests for [[SPC-S-Conditional.else]]
  - [[.else_inherit]] We should correctly inherit sensitivity
    from the parent conditional.
    e.g. in 
    ```make
    ifdef FOO
      a := YES 
    else
      a := NO
      b := NO
    endif
    ```
    After the `else` branch, we should still be sensitive
    to `FOO`
  - [[.else_chaining]]
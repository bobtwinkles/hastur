# REQ-Location
partof:
- REQ-expansion-tracking
- REQ-interface
###
The library shall expose a type representing a source location.
This shall be done in such a way as to allow interning of source locations in the future.
Since the source of a segment
might be directly not be directly present in
the source makefiles,
and instead come from an `$(eval ...)` block.
Thus,
our source location type must support pointing into
the result of an `$(eval ...)`
in addition to raw content source locations.
Therefore the two types of source location are
  - [[.raw_location]] for source locations in actual files
  - [[.eval_location]] for source locations from `eval` blocks.
  
[[.comparable]] Source locations shall implement `PartialOrd`.
Locations are not comparable across files or between `eval` blocks.


# SPC-Location
The `SourceLocation` type shall be implemented as
an enumeration with two variants.
[[.raw_location]] Raw locations shall be represented by
a structure tracking line, character, and source file name.
All fields in this structure shall be private.
[[.eval_location]] Similarly, eval locations
shall be represented by a structure
with private fields:
one pointing to the root of the evaluation tree,
another representing the number of leaf nodes
preceding the location,
and a final field representing the
character offset into the leaf node.
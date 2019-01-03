# SPC-File-List
partof:
- SPC-P-Directives
- SPC-P-Rule
- SPC-V-AST
###
In GNU Make, this is implemented by `read.c:parse_file_seq`.

TODO: This requires further reverse engineering.
It's going to involve some ugly stuff
w.r.t. duplicating shell globing features and
the `ar_glob` function.
Consult `read.c:parse_file_seq` for an example of this in action

Things I know:
  - [[.tilde]]
    If there is a `~`, expand it
  - [[.archive]]
    If there is a `(`, we're dealing with an archive group
    - [[.archive_simple]]
      Archive groups can be simple (just one element) like
      `libmy_archive.a(x.o)`
    - [[.archive_complex]]
      They can also be complex, like
      `libmy_archive.a(x.o y.o)`.
      In this case,
      the parser shall instantiate several copies of the group
      to turn them into simple archive groups.
      E.G. we should output something like
      `libmy_archive.a(x.o)`
      `libmy_archive.a(y.o)`
  - [[.simple]]
    If we just have a normal filename after all this expansion
    (no `*?[` characters),
    just add it as a target.
    Note that this applies to all archive targets as well,
    individually.
  - [[.glob]]
    If one of `*?[` appeared,
    we need to glob.
    This is the part that needs more reverse engineering


# SPC-Paths
partof: REQ-Interface
###

Consult [the GNU Make documentation on paths](https://www.gnu.org/software/make/manual/make.html#Directory-Search) for details.

TODO: flesh this out
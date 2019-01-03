# REQ-parse-control
partof: REQ-interface
###
There are various options that determine how Make parses files.
We will support these using the builder pattern,
by providing a `MakefileParseOptions`
structure that
encapsulates all of these toggles.
These are as follows:

## [[.windows_compat]]
[[.line_endings]] We need to support both Windows and UNIX line endings (`\r\n` and `\n`).
It is unlikely that always accepting both will cause problems,
but there may need to be a toggle for it.
[[.path_sep]] When in windows compatibility mode,
we should use ';' rather than ':' as the path separator

### Testing
 - Since acording to [[TST-architecture]] all tests use UNIX line endings, we won't test for the explicitly.
 - [[.tst_windows_line_endings]] An explicit test to make sure our line ending system supports Windows line endings.

## [[.posix_compat_continuations]]
POSIX and GNU make have different conventions about line continuations.
In GNU make, all whitespace around the continuation is collapsed into a single space.
In POSIX make, only the `\\\n` substring is replaced with a single space.
That is, if we consider the following variable assignment
```make
foo := bar\
     baz
```
GNU make will set
`foo := bar baz`
while POSIX make will set
`foo := bar     baz`.
We shall provide an option in
the parse control structure which allows
configuring this behavior.

### Testing
 - [[.tst-posix-continuations]] We should support the POSIX line continuation behavior
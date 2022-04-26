A Lexer for the TableGen language (https://llvm.org/docs/TableGen/)

The lexer in this root directory takes a `[]byte` buffer and returns `string`
tokens, incurring allocations. The lexer in the `lexer-string` subdirectory
takes a `string` and returns `string` subslices as tokens, with much less
allocation.

To run tests and benchmarks, set the `TDINPUT` env var to point to the `.td`
file in this directory.

Implements a tablegen lexer using two approaches.

"owning": returns owned Strings as tokens

"slice": returns borrowed subslices into the input slice

The lexer parses the same syntax as described in
https://eli.thegreenplace.net/2014/03/27/rewriting-the-lexer-benchmark-in-go and
earlier posts.

This lexer itself has no dependencies; the only dependency in this project is on
the `criterion` crate used for benchmarking.

To run the tests and benchmarks, point the `TDINPUT` environment variable to
the 1 MiB TableGen source file used in the lexer series of posts. It's available
on GitHub here: https://github.com/eliben/code-for-blog/blob/master/2014/tablegen-lexer-go/input.td

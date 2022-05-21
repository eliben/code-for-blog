Implements a tablegen lexer using two approaches.

"owning": returns owned Strings as tokens

"slice": returns borrowed subslices into the input slice

The lexer parses the same syntax as described in
https://eli.thegreenplace.net/2014/03/27/rewriting-the-lexer-benchmark-in-go and
earlier posts.

This lexer itself has no dependencies; the only dependency in this project is on
the `criterion` crate used for benchmarking.

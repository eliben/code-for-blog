# BPE in Go

Implementation of Byte Pair Encoding in Go, with a web UI playground.

A real-world vocabulary is included in `data` - it's downloaded from OpenAI
(link is found in the source code of tiktoken).

See the tests for usage - `tiktoken_tokenize_test.go` is an end-to-end test.

For the web UI, `cd cmd/wasm`, then run `make build`. Then run `make serve`
and follow instructions. A live instance is also running at
https://eliben.org/bpe

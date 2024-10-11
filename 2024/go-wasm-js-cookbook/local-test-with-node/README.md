See the `Makefile` for invocations. `make run` and `make test` do the work.

An alternative to passing `-exec` explicitly is adding `supportfiles` to $PATH.

----

To understand how it works, check out `go help run`:

    By default, 'go run' runs the compiled binary directly: 'a.out arguments...'.
    If the -exec flag is given, 'go run' invokes the binary using xprog:
      'xprog a.out arguments...'.
    If the -exec flag is not given, GOOS or GOARCH is different from the system
    default, and a program named go_$GOOS_$GOARCH_exec can be found
    on the current search path, 'go run' invokes the binary using that program,
    for example 'go_js_wasm_exec a.out arguments...'. This allows execution of
    cross-compiled programs when a simulator or other execution method is
    available.

For running with `GOOS=js GOARCH=wasm`, the relevant executor is `go_js_wasm_exec`.
This is a bash script that runs the compiled binary in `node` along with
the support file `wasm_exec_node.js`, which instantiates the WebAssembly
binary and hooks things up a bit; specifically, this support file also loads
`wasm_exec.js` that hooks up things like stdio, so our code can print
things, etc.

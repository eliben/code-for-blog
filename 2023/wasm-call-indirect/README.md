To compile the WAT file into WASM:

```
$ wat2wasm table.wat
```

This creates a ``table.wasm`` file that the JS code loads. Once that's done,
run `node table.js`.

### go-env

The `go-env` directory reproduces the same sample using Go and the
[wazero](https://wazero.io/) runtime. It expects the same `table.wasm` file
to be present in the `go-env` directory (because `go:embed` directives cannot
use `..`), so to run this sample:

1. Copy `table.wasm` into the `go-env` directory
2. In the `go-env` directory, run `go run .`

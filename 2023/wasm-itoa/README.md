Shows how to write an `itoa` in WAT; imports and tests it from JS.

First, compile the WAT file to WASM with:

```
$ wat2wasm itoa.wat
```

Then, load and test the resulting `itoa.wasm` with:

```
$ node test.js
```


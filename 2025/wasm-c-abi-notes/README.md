To compile standalone C files with WASM output, some options:

```
clang -O -S -c -mllvm --x86-asm-syntax=intel -fno-verbose-asm -target wasm32-unknown-unknown try.c
```

Creates `try.s` with WASM text in assembly-like sections.

```
clang -O -c -target wasm32-unknown-unknown try.c
```

Creates `try.o` with WASM binary, which can be handled as usual, e.g. with
`wasm-tools print try.o` (or using `dump`).

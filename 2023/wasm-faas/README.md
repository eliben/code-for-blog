See `build.sh` for how the examples are compiled to `.wasm` modules. It assumes
some tools like `wat2wasm`, `tinygo` and `cargo` are installed and in PATH.

When everything is built, run `go run .` in the root directory; then interact
with the server using `curl`:

```
$ curl "localhost:8080/watenv/foobar?foo=bar&asdf=11111&baz=5"
```

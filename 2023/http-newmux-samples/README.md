Both Go files in this directory have `main` functions, so they should be
run with `go run <filename.go>`.

To run this sample before Go 1.22 is released, it's recommended to use `gotip`.
To ensure that the right toolchain is invoked, it can be a good idea to set
`GOTOOLCHAIN=local`:

    GOTOOOLCHAIN=local gotip run sample.go
    GOTOOOLCHAIN=local gotip run sample-conflict.go

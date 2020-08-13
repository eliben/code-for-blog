// Embed interfaces with overlapping method sets
// see https://github.com/golang/proposal/blob/master/design/6977-overlapping-interfaces.md
package main

import (
	"fmt"
	"io"
)

// This will work fine on Go 1.14+, but will fail with earlier versions of
// Go with the error "duplicate method Close", since Close is a method in both
// ReadCloser and WriteCloser.
//
// From the (updated) spec:
//
//   An interface T may use a (possibly qualified) interface type name E in
//   place of a method specification. This is called embedding interface E in T.
//   The method set of T is the union of the method sets of T’s explicitly
//   declared methods and of T’s embedded interfaces.
//
//   A union of method sets contains the (exported and non-exported) methods of
//   each method set exactly once, and methods with the same names must have
//   identical signatures.
type ReadWriteCloser interface {
	io.ReadCloser
	io.WriteCloser
}

func main() {
	fmt.Println("hi")
}

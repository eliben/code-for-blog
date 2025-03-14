// Example: iterating over a slice backwards.
// This essentially re-implements the standard library function slices.Backward
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"fmt"
	"iter"
)

func main() {
	s := []int{5, 6, 7, 8, 11, 22}

	for _, e := range Backward(s) {
		fmt.Println(e)
	}
}

func Backward[E any](x []E) iter.Seq2[int, E] {
	return func(yield func(int, E) bool) {
		i := len(x) - 1
		for i >= 0 && yield(i, x[i]) {
			i--
		}
	}
}

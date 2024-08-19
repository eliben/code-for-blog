// Example: iterating over "all" Fibonacci numbers.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import "fmt"

func main() {
	for p := range genFib {
		fmt.Println(p)

		if p > 1000 {
			break
		}
	}
}

func genFib(yield func(int) bool) bool {
	a, b := 1, 1

	for {
		if !yield(a) {
			return false
		}
		a, b = b, a+b
	}

	return true
}

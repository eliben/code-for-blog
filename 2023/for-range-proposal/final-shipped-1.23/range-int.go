// Example of new range-over-int functionality.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import "fmt"

func main() {
	for i := range 5 {
		fmt.Println(i)
	}

	n := 4
	for i := range n {
		fmt.Printf("i=%d, n=%d\n", i, n)
	}

	for range n {
		fmt.Println("beep")
	}
}

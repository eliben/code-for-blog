// Closure capture of loop variables.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"fmt"
	"time"
)

func foobyval(n int) {
	fmt.Println(n)
}

func main() {
	for i := 0; i < 5; i++ {
		go func() {
			foobyval(i)
		}()
	}

	time.Sleep(100 * time.Millisecond)
}

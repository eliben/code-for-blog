// Basic Go example compiled to WASM.
package main

import (
	"fmt"
	"os"
)

func main() {
	fmt.Println("goenv environment:")

	for _, e := range os.Environ() {
		fmt.Println(" ", e)
	}
}

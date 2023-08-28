// Basic Go example compiled to WASM.
package main

import (
	"fmt"
	"os"
)

// Show how to import functions from the host using go:wasmimport

//go:wasmimport env log_i32
func logInt(i int32)

func main() {
	logInt(42)
	fmt.Println("goenv environment:")

	for _, e := range os.Environ() {
		fmt.Println(" ", e)
	}
}

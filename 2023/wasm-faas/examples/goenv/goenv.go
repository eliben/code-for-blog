// Basic Go example compiled to WASM.
package main

import (
	"fmt"
	"os"
	"unsafe"
)

//go:wasm-module env
//export log_string
func host_log(ptr uint32, size uint32)

func main() {
	host_log(stringToPtr("logging from goenv"))

	fmt.Println("goenv environment:")

	for _, e := range os.Environ() {
		fmt.Println(" ", e)
	}
}

// stringToPtr returns a pointer and size pair for the given string in a way
// compatible with WebAssembly numeric types.
func stringToPtr(s string) (uint32, uint32) {
	buf := []byte(s)
	ptr := &buf[0]
	unsafePtr := uintptr(unsafe.Pointer(ptr))
	return uint32(unsafePtr), uint32(len(buf))
}

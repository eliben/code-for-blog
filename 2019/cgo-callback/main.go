// Main sample.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
package main

/*
#include <stdlib.h>
#include "clibrary.h"

#cgo CFLAGS: -I .
#cgo LDFLAGS: -L . -lclibrary

// Since main.go has //export directives we can't place function definitions in
// it - we'll get multiple definition errors from the linker (see
// https://golang.org/cmd/cgo/#hdr-C_references_to_Go for more on this
// limitation). We can't mark them 'static inline' either because we're taking
// their address to pass to clibrary; thus, they are moved to a separate Go
// file.

extern void startCgo(void*, int);
extern void endCgo(void*, int, int);
*/
import "C"
import (
	"fmt"
	"unsafe"
)

import gopointer "github.com/mattn/go-pointer"

type Visitor interface {
	Start(int)
	End(int, int)
}

func GoTraverse(filename string, v Visitor) {
	cCallbacks := C.Callbacks{}

	cCallbacks.start = C.StartCallbackFn(C.startCgo)
	cCallbacks.end = C.EndCallbackFn(C.endCgo)

	// Allocate a C string to hold the contents of filename, and free it up when
	// we're done.
	var cfilename *C.char = C.CString(filename)
	defer C.free(unsafe.Pointer(cfilename))

	// Create an opaque C pointer for the visitor to pass ot traverse.
	p := gopointer.Save(v)
	defer gopointer.Unref(p)

	C.traverse(cfilename, cCallbacks, p)
}

//export goStart
func goStart(user_data unsafe.Pointer, i C.int) {
	v := gopointer.Restore(user_data).(Visitor)
	v.Start(int(i))
}

//export goEnd
func goEnd(user_data unsafe.Pointer, a C.int, b C.int) {
	v := gopointer.Restore(user_data).(Visitor)
	v.End(int(a), int(b))
}

type MyVisitor struct {
	startState int
}

func (mv *MyVisitor) Start(i int) {
	mv.startState = i
	fmt.Println("End:", i)
}

func (mv *MyVisitor) End(a, b int) {
	fmt.Printf("Start: %v %v [state = %v]\n", a, b, mv.startState)
}

func main() {
	mv := &MyVisitor{startState: 0}
	GoTraverse("joe", mv)
}

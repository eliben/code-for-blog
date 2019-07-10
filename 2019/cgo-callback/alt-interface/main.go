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

import cpointer "github.com/mattn/go-pointer"

type GoStartCallback func(int)
type GoEndCallback func(int, int)

type GoCallbacks struct {
	startCb GoStartCallback
	endCb   GoEndCallback
}

func GoTraverse(filename string, cbs *GoCallbacks) {
	cCallbacks := C.Callbacks{}

	if cbs.startCb != nil {
		cCallbacks.start = C.StartCallbackFn(C.startCgo)
	}
	if cbs.endCb != nil {
		cCallbacks.end = C.EndCallbackFn(C.endCgo)
	}

	// Allocate a C string to hold the contents of filename, and free it up when
	// we're done.
	var cfilename *C.char = C.CString(filename)
	defer C.free(unsafe.Pointer(cfilename))

	// Create an opaque C pointer for cbs to pass ot traverse.
	p := cpointer.Save(cbs)
	defer cpointer.Unref(p)

	C.traverse(cfilename, cCallbacks, p)
}

//export goStart
func goStart(user_data unsafe.Pointer, i C.int) {
	gcb := cpointer.Restore(user_data).(*GoCallbacks)
	gcb.startCb(int(i))
}

//export goEnd
func goEnd(user_data unsafe.Pointer, a C.int, b C.int) {
	gcb := cpointer.Restore(user_data).(*GoCallbacks)
	gcb.endCb(int(a), int(b))
}

func main() {
	cb := &GoCallbacks{
		startCb: func(i int) { fmt.Println("from go start", i) },
		endCb:   func(a, b int) { fmt.Println("from go end", a, b) },
	}
	GoTraverse("joe", cb)

	// Another traverse, with state
	var state int
	cb = &GoCallbacks{
		startCb: func(i int) { state = i },
		endCb:   func(a, b int) { fmt.Println("end; state =", state) },
	}
	GoTraverse("joe", cb)
}

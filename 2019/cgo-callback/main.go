package main

/*
#include "clibrary.h"

#cgo CFLAGS: -I .
#cgo LDFLAGS: -L . -lclibrary

// Forward declarations of functions defined in another file.
// See https://golang.org/cmd/cgo/#hdr-C_references_to_Go
extern void startCgo(void* data, int i);
extern void endCgo(void* data, int a, int b);
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

func GoTraverse(cbs *GoCallbacks) {
	cCallbacks := C.Callbacks{}

	if cbs.startCb != nil {
		cCallbacks.start = C.StartCallbackFn(C.startCgo)
	}
	if cbs.endCb != nil {
		cCallbacks.end = C.EndCallbackFn(C.endCgo)
	}

	p := cpointer.Save(cbs)
	C.traverse(p, cCallbacks)
	cpointer.Unref(p)
}

//export goStart
func goStart(data unsafe.Pointer, i C.int) {
	gcb := cpointer.Restore(data).(*GoCallbacks)
	gcb.startCb(int(i))
}

//export goEnd
func goEnd(data unsafe.Pointer, a C.int, b C.int) {
	gcb := cpointer.Restore(data).(*GoCallbacks)
	gcb.endCb(int(a), int(b))
}

func main() {
	cb := &GoCallbacks{
		startCb: func(i int) { fmt.Println("from go start", i) },
		endCb:   func(a, b int) { fmt.Println("from go end", a, b) },
	}
	GoTraverse(cb)

	// Another traverse, with state
	var state int
	cb = &GoCallbacks{
		startCb: func(i int) { state = i },
		endCb:   func(a, b int) { fmt.Println("end; state =", state) },
	}
	GoTraverse(cb)
}

//go:build js && wasm

package main

import (
	"log"
	"syscall/js"
	"testing"
)

func TestJSArr(t *testing.T) {
	log.Println("hello from test in js/wasm")

	objs := js.Global().Call("eval", `({
arr: [41,42,43],
})`)

	arr := objs.Get("arr")
	if got := arr.Length(); got != 3 {
		t.Errorf("got %#v, want %#v", got, 3)
	}

	if got := arr.Index(1).Int(); got != 42 {
		t.Errorf("got %#v, want %#v", got, 42)
	}
}

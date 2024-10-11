//go:build js && wasm

package main

import (
	"log"
	"syscall/js"
)

func main() {
	log.Println("hello from main in js/wasm")

	objs := js.Global().Call("eval", `({
arr: [41,42,43],
})`)
	arr := objs.Get("arr")

	for i := 0; i < arr.Length(); i++ {
		log.Println(arr.Index(i).Int())
	}
}

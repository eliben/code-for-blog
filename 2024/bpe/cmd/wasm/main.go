package main

import (
	"syscall/js"
)

func main() {
	js.Global().Set("itsAlive", jsItsAlive)

	// For the Go code to be usable from JS, the main function has to run forever.
	<-make(chan bool)
}

var jsItsAlive = js.FuncOf(func(this js.Value, args []js.Value) interface{} {
	result := itsAlive()
	return result
})

func itsAlive() string {
	return "go is alive"
}

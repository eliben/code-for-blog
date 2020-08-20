// Show how embedding an interface in a struct makes the struct automatically
// implement the interface; but unless the embedded field is initialized with
// something, actual calls to these methods will panic.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import "fmt"

type Fooer interface {
	Foo() string
}

type Container struct {
	Fooer
}

func sink(f Fooer) {
	fmt.Println("sink:", f.Foo())
}

type TheRealFoo struct {
}

func (trf TheRealFoo) Foo() string {
	return "TheRealFoo Foo"
}

func main() {
	// Create a container without initializing the embedded slot. So it's the
	// nil value of an interface now.
	co := Container{}
	_ = co

	// This compiles but panics at runtime: nil dereference when trying to call
	// the interface's function.
	sink(co)

	// This will actually work fine.
	co2 := Container{Fooer: TheRealFoo{}}
	sink(co2)
}

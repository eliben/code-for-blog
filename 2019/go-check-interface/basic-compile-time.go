// Basic example that demonstrates a type that doesn't implement an interface
// being flagged for a compile error when assigned to the interface.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

type Munger interface {
	Munge(int)
}

type Foo int
type Bar int

func (f Foo) Munge(int) {
}

var _ Munger = (*Bar)(nil)

func main() {
	var m Munger
	var f Foo
	m = f

	//var b Bar
	//m = b

	var i int
	var mm Munger = i
	//m = i
}

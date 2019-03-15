// Check whether a type implements an interface at run-time, using a conversion
// to interface{} and a type assertion.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import "fmt"

type Munger interface {
	Munge(int)
}

type Foo int
type Bar int

func (f Foo) Munge(int) {
}

func main() {

	// compile error
	//_, ok := f.(Munger)

	//var f Foo
	//_, ok := interface{}(f).(Munger)
	//fmt.Println(ok)

	var b Bar
	_, ok := interface{}(b).(Munger)
	fmt.Println(ok)
}

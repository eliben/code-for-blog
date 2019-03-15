// Basic example of run-time check with an interface{} conversion.
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

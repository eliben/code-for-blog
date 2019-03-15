// Run-time check using reflect.
package main

import (
	"fmt"
	"reflect"
)

type Munger interface {
	Munge(int)
}

type Foo int
type Bar int

func (f Foo) Munge(int) {
}

func main() {
	var f Foo

	iMunger := reflect.TypeOf((*Munger)(nil)).Elem()
	ok := reflect.TypeOf(&f).Implements(iMunger)

	fmt.Println(ok)

	var b Bar
	ok = reflect.TypeOf(&b).Implements(iMunger)
	fmt.Println(ok)
}

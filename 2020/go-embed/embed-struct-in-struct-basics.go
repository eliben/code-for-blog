// Basic example of embedding a struct in a struct.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import "fmt"

type Base struct {
	b int
}

func (base Base) Describe() string {
	return fmt.Sprintf("base %d belongs to us", base.b)
}

type Container struct {
	Base
	c string
}

func main() {
	co := Container{}
	co.b = 1
	co.c = "string"
	fmt.Printf("co -> {b: %v, c: %v}\n", co.b, co.c)

	// Note: using "Base" in this literal to initialize the field.
	cc := Container{Base: Base{b: 10}, c: "foo"}
	fmt.Printf("cc -> {b: %v, c: %v}\n", cc.b, cc.c)

	// Fully qualified access.
	fmt.Println(cc.Base.b)

	fmt.Println(cc.Describe())
}

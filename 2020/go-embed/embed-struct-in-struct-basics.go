package main

import "fmt"

type Base struct {
	b int
}

type Container struct {
	Base
	c string
}

func main() {
	bb := Base{b: 10}

	// Note: using "Base" in this literal to initialize the field.
	cc := Container{Base: bb, c: "foo"}

	fmt.Printf("cc -> {b: %v, c: %v}\n", cc.b, cc.c)
}

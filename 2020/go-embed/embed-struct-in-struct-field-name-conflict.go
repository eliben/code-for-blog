// Demonstrates field name shadowing with embedding.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import "fmt"

type Base struct {
	b   int
	tag string
}

func (base Base) DescribeTag() string {
	return fmt.Sprintf("tag is %s", base.tag)
}

type Container struct {
	Base
	c   string
	tag string
}

func (co Container) DescribeTag() string {
	return fmt.Sprintf("tag is %s", co.tag)
}

func main() {
	b := Base{b: 10, tag: "b's tag"}
	co := Container{Base: b, c: "foo", tag: "co's tag"}

	fmt.Println(b.DescribeTag())
	fmt.Println(co.DescribeTag())
	fmt.Println(co.Base.DescribeTag())

	fmt.Println(co.Base.tag)
}

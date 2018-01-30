// Expression problem solution in Go with interfaces.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.

package main

import (
	"fmt"
	"strconv"
)

type Expr interface {
}

type Constant struct {
	value float64
}

type BinPlus struct {
	left  Expr
	right Expr
}

type Eval interface {
	Eval() float64
}

type Stringify interface {
	ToString() string
}

func (c *Constant) Eval() float64 {
	return c.value
}

func (bp *BinPlus) Eval() float64 {
	return bp.left.(Eval).Eval() + bp.right.(Eval).Eval()
}

func (c *Constant) ToString() string {
	return strconv.FormatFloat(c.value, 'E', -1, 64)
}

func (bp *BinPlus) ToString() string {
	// The moment of truth is here... bp.left is an Expr, which does not
	// have a ToString method. Obviously this will only work if left and right
	// implement the Stringable interface. The type assertion makes this
	// expectation explicit and will panic otherwise.
	ls := bp.left.(Stringify)
	rs := bp.right.(Stringify)
	return fmt.Sprintf("%s + %s", ls.ToString(), rs.ToString())
}

func main() {
	fmt.Println("booya")

	// constants
	c := Constant{value: 26.4}

	fmt.Printf("c Eval = %g\n", c.Eval())
	fmt.Printf("c ToString = %s\n", c.ToString())

	c11 := Constant{value: 1.1}
	c22 := Constant{value: 2.2}
	c33 := Constant{value: 3.3}
	bp := BinPlus{left: &BinPlus{left: &c11, right: &c22}, right: &c33}

	fmt.Printf("bp Eval = %g\n", bp.Eval())
	fmt.Printf("bp ToString = %s\n", bp.ToString())
}

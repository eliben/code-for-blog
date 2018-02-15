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

// Types
type Constant struct {
	value float64
}

type BinaryPlus struct {
	left  Expr
	right Expr
}

// Functions wrapped in interfaces
type Eval interface {
	Eval() float64
}

type Stringify interface {
	ToString() string
}

func (c *Constant) Eval() float64 {
	return c.value
}

func (c *Constant) ToString() string {
	return strconv.FormatFloat(c.value, 'f', -1, 64)
}

// As far as the compiler is concerned, bp.left is an Expr. Expr doesn't have an
// Eval method. Therefore, a cast is required - it will fail at runtime if
// bp.left doesn't implement Eval.
func (bp *BinaryPlus) Eval() float64 {
	return bp.left.(Eval).Eval() + bp.right.(Eval).Eval()
}

func (bp *BinaryPlus) ToString() string {
	ls := bp.left.(Stringify)
	rs := bp.right.(Stringify)
	return fmt.Sprintf("(%s + %s)", ls.ToString(), rs.ToString())
}

// Now adding a new type...

type BinaryMul struct {
	left  Expr
	right Expr
}

func (bm *BinaryMul) Eval() float64 {
	return bm.left.(Eval).Eval() * bm.right.(Eval).Eval()
}

func (bm *BinaryMul) ToString() string {
	ls := bm.left.(Stringify)
	rs := bm.right.(Stringify)
	return fmt.Sprintf("(%s * %s)", ls.ToString(), rs.ToString())
}

// Function that emulates creating a new expression from some input. It has to
// return Expr, which should then be casted with a type assertion.
func CreateNewExpr() Expr {
	c11 := Constant{value: 1.1}
	c22 := Constant{value: 2.2}
	c33 := Constant{value: 3.3}
	bp := BinaryPlus{left: &BinaryPlus{left: &c11, right: &c22}, right: &c33}
	return &bp
}

func main() {
	fmt.Println("hello")

	// constants
	c := Constant{value: 26.4}

	fmt.Printf("c Eval = %g\n", c.Eval())
	fmt.Printf("c ToString = %s\n", c.ToString())

	c11 := Constant{value: 1.1}
	c22 := Constant{value: 2.2}
	c33 := Constant{value: 3.3}
	bp := BinaryPlus{left: &BinaryPlus{left: &c11, right: &c22}, right: &c33}

	// Uncomment this code - it will fail at run-time.
	bp1 := BinaryPlus{left: "john", right: Constant{value: 2.2}}
	fmt.Printf("bp1 Eval = %g\n", bp1.Eval())

	fmt.Printf("bp Eval = %g\n", bp.Eval())
	fmt.Printf("bp ToString = %s\n", bp.ToString())

	ne := CreateNewExpr()
	fmt.Printf("ne Eval = %g\n", ne.(Eval).Eval())
	fmt.Printf("ne ToString = %s\n", ne.(Stringify).ToString())

	bm := BinaryMul{left: &bp, right: &c22}

	fmt.Printf("bm Eval = %g\n", bm.Eval())
	fmt.Printf("bm ToString = %s\n", bm.ToString())
}

package main

func foo(int) int

func main() {
	var x, y, z int

	m := x + foo(y*z)
	m = y + z*x
	m = x * y

	_ = m
}

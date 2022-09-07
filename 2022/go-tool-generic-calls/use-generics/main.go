package main

import "golang.org/x/exp/slices"

func foo() {
	var fs []float64

	newfs := slices.Clone(fs)
	handle(newfs, len(newfs))
}

func bar() {
	var fs []string
	handle[string, int](fs, 10)
}

func main() {
	foo()
	bar()
}

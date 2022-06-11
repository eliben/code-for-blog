// Y combinator in Go, using generics.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import "fmt"

type Func[T, U any] func(T) U
type TagFunc[T, U any] func(Func[T, U]) Func[T, U]
type CombinatorFunc[T, U any] func(CombinatorFunc[T, U]) Func[T, U]

func Y[T, U any](f TagFunc[T, U]) Func[T, U] {
	return func(self CombinatorFunc[T, U]) Func[T, U] {
		return f(func(n T) U {
			return self(self)(n)
		})
	}(func(self CombinatorFunc[T, U]) Func[T, U] {
		return f(func(n T) U {
			return self(self)(n)
		})
	})
}

var factorial_tag = func(recurse Func[int, int]) Func[int, int] {
	return func(n int) int {
		if n == 0 {
			return 1
		}
		return n * recurse(n-1)
	}
}

var fib_tag = func(recurse Func[int, int]) Func[int, int] {
	return func(n int) int {
		if n <= 1 {
			return n
		}
		return recurse(n-1) + recurse(n-2)
	}
}

type Node struct {
	val   int
	left  *Node
	right *Node
}

var treesum_tag = func(recurse Func[*Node, int]) Func[*Node, int] {
	return func(n *Node) int {
		if n == nil {
			return 0
		} else {
			return n.val + recurse(n.left) + recurse(n.right)
		}
	}
}

func main() {
	fac := Y(factorial_tag)
	fib := Y(fib_tag)

	for i := 0; i <= 5; i++ {
		fmt.Printf("fac(%v) = %v\n", i, fac(i))
		fmt.Printf("                  fib(%v) = %v\n", i, fib(i))
	}

	tree := &Node{
		val:  1,
		left: &Node{val: 2},
		right: &Node{
			val:   4,
			left:  &Node{val: 3},
			right: &Node{val: 7}},
	}

	treesum := Y(treesum_tag)
	fmt.Printf("tree sum: %v\n", treesum(tree))
}

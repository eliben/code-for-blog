// Example: in-order iterator over a binary tree.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"fmt"
	"iter"
)

func main() {
	// Create a sample tree:
	//
	//       10
	//      /  \
	//     20  40
	//    /  \
	//   30  39
	tt := &Tree[int]{
		10,
		&Tree[int]{
			20,
			&Tree[int]{30, nil, nil},
			&Tree[int]{39, nil, nil}},
		&Tree[int]{40, nil, nil},
	}

	for v := range tt.Inorder() {
		fmt.Println(v)
	}
}

// Tree is a binary tree.
type Tree[E any] struct {
	val         E
	left, right *Tree[E]
}

// Inorder iterates over the tree, in-order.
func (t *Tree[E]) Inorder() iter.Seq[E] {
	return func(yield func(E) bool) {
		t.push(yield)
	}
}

func (t *Tree[E]) push(yield func(E) bool) bool {
	if t == nil {
		return true
	}
	return t.left.push(yield) && yield(t.val) && t.right.push(yield)
}

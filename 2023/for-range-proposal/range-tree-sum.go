package main

import "fmt"

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

	sum := 0
	for v := range tt.All {
		sum += v
	}
	fmt.Println(sum)
}

// Tree is a binary tree.
type Tree[E any] struct {
	val         E
	left, right *Tree[E]
}

// All may be used in a for/range loop to iterate
// through all the values of the tree.
// This implementation does an in-order traversal.
func (t *Tree[E]) All(yield func(E) bool) bool {
	if t == nil {
		return true
	}

	// As soon as a yield returns false, All returns false too, to avoid
	// iterating more.
	return t.left.All(yield) && yield(t.val) && t.right.All(yield)
}

// Binary tree construction from inorder-depth representation.
//
// The main() here is just a small experiment; to exercise the code run
// `go test`.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"fmt"
	"log"
	"strings"
)

func main() {
	dl := Parse("[[4,8],[3,5]]")
	t := dl.BuildTree()
	t.Verify()
	fmt.Println(t)
}

// "Depth-list" representation of nested pairs: a slice of DItem, each of which
// is a value and its depth in the pairs tree.
type DItem struct {
	Value int
	Depth int
}

type DList []DItem

func (dl DList) String() string {
	var parts []string
	for _, di := range dl {
		parts = append(parts, fmt.Sprintf("(%v %v)", di.Value, di.Depth))
	}
	return strings.Join(parts, " ")
}

// Parse parses a textual representation of nested paisr into a DList.
// Nested pairs are wrapped in [...] and separated by commas; for example,
// this is a nested pair: "[[1,2],[[3,4],5]]"
// Bad inputs are not checked and will likely produce wrong output.
func Parse(s string) DList {
	depth := 0
	var lst DList

	for _, c := range strings.TrimSpace(s) {
		switch c {
		case '[':
			depth++
		case ']':
			depth--
		case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
			lst = append(lst, DItem{int(c) - '0', depth})
		default:
		}
	}

	return lst
}

// BuildTree builds a Tree from a DList using an iterative algorithm.
func (dl DList) BuildTree() *Tree {
	if len(dl) == 0 {
		return nil
	}
	// result is the tree this function is building. The result pointer always
	// points at the root, so we can return it to the caller. t points to the
	// current node being constructed throughout the algorithm.
	result := &Tree{}
	t := result

	// depth is the current depth of t's children.
	depth := 1

	// stack of parent nodes to implement backtracking up the tree once we're done
	// with a subtree.
	var stack []*Tree

	// The outer loop iterates over all the items in a DList, inserting each one
	// into the tree. Loop invariant: all items preceding this item in dl have
	// already been inserted into the tree, and t points to the node where the
	// last insertion was made.
nextItem:
	for _, item := range dl {
		// The inner loop finds the right place for item in the tree and performs
		// insertion.
		// Loop invariant: t points at the node where we're trying to insert, depth
		// is the depth of its children and stack holds a stack of t's parents.
		for {
			// Check if item can be inserted as a child of t; this can be done only if
			// our depth matches the item's and t doesn't have both its children yet.
			// Otherwise, t is not the right place and we have to keep looking.
			if item.Depth == depth && t.Left == nil {
				t.Left = &Tree{Value: item.Value}
				continue nextItem
			} else if item.Depth == depth && t.Right == nil {
				t.Right = &Tree{Value: item.Value}
				continue nextItem
			}

			// We can't insert at t.
			// * If t does not have a left child yet, create it and repeat loop with
			//   this left child as t.
			// * If t does not have a right child yet, create it and repeat loop with
			//   this right child as t.
			// * If t has both children, we have to backtrack up the tree to t's
			//   parent.
			if t.Left == nil {
				stack = append(stack, t)
				t.Left = &Tree{}
				t = t.Left
				depth++
			} else if t.Right == nil {
				stack = append(stack, t)
				t.Right = &Tree{}
				t = t.Right
				depth++
			} else {
				// Pop from the stack to make t point to its parent
				t, stack = stack[len(stack)-1], stack[:len(stack)-1]
				depth--
			}
		}
	}

	return result
}

// BuildTreeRec builds a Tree from a DList using a recursive algorithm.
func (dl DList) BuildTreeRec() *Tree {
	cursor := 0

	var builder func(depth int) *Tree
	builder = func(depth int) *Tree {
		if cursor >= len(dl) {
			return nil
		}

		var left *Tree
		if dl[cursor].Depth == depth {
			left = &Tree{Value: dl[cursor].Value}
			cursor++
		} else {
			left = builder(depth + 1)
		}

		var right *Tree
		if dl[cursor].Depth == depth {
			right = &Tree{Value: dl[cursor].Value}
			cursor++
		} else {
			right = builder(depth + 1)
		}
		return &Tree{Left: left, Right: right}
	}

	return builder(1)
}

type Tree struct {
	Value       int
	Left, Right *Tree
}

// Verify checks that the tree is structured as expected: internal nodes have a
// default value and two children; leaves have a non-default value and no
// children.
func (t *Tree) Verify() {
	if t == nil {
		return
	}

	if t.Left == nil && t.Right == nil {
		if t.Value == 0 {
			log.Fatalf("want leaf with value 0, got %v", t.Value)
		}
	} else if t.Left != nil && t.Right != nil {
		if t.Value != 0 {
			log.Fatalf("want internal node with value !=0, got %v", t.Value)
		}
		t.Left.Verify()
		t.Right.Verify()
	} else {
		log.Fatalf("want either 0 or 2 children in node, got 1")
	}
}

func (t *Tree) String() string {
	if t == nil {
		return "()"
	} else if t.Left == nil && t.Right == nil {
		return fmt.Sprint(t.Value)
	}

	// Assume the tree has been verified: we have both left and right children.
	return "(" + t.Left.String() + " " + t.Right.String() + ")"
}

// Implementing the Tree ADT (Algebraic Data Type) example from
// https://en.wikipedia.org/wiki/Algebraic_data_type
//
// The Haskell equivalent is:
//
// data Tree = Empty
//           | Leaf Int
//           | Node Tree Tree
//
// depth :: Tree -> Int
// depth Empty = 0
// depth (Leaf n) = 1
// depth (Node l r) = 1 + max (depth l) (depth r)
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"fmt"
	"log"
)

func max(x, y int) int {
	if x > y {
		return x
	}
	return y
}

type Tree interface {
}

type Empty struct {
}

type Leaf struct {
	v int
}

type Node struct {
	left, right Tree
}

func depth(t Tree) int {
	switch nt := t.(type) {
	case Empty:
		return 0
	case Leaf:
		return 1
	case Node:
		return 1 + max(depth(nt.left), depth(nt.right))
	default:
		log.Fatalf("unexpected type %T", nt)
	}
	return 0
}

func main() {
	t := Node{left: Leaf{v: 2}, right: Node{left: Empty{}, right: Leaf{v: 5}}}
	fmt.Println(depth(t))
}

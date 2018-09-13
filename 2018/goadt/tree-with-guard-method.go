// Same as tree.go, but adding a guard method on the Tree inteface to help
// type checking.
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
	isTree()
}

type Empty struct {
}

type Leaf struct {
	v int
}

type Node struct {
	left, right Tree
}

func (_ Leaf) isTree()  {}
func (_ Node) isTree()  {}
func (_ Empty) isTree() {}

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

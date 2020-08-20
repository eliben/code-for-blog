// Demonstrates how to use sort, and how to reverse-sort using an embedded
// interface.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"fmt"
	"sort"
)

// This replicates what's already in the sort package. Instead of Reverse,
// we can use sort.Reverse.
type reverse struct {
	sort.Interface
}

func (r reverse) Less(i, j int) bool {
	return r.Interface.Less(j, i)
}

func Reverse(data sort.Interface) sort.Interface {
	return &reverse{data}
}

func main() {
	lst := []int{4, 5, 2, 8, 1, 9, 3}
	sort.Sort(sort.IntSlice(lst))
	fmt.Println(lst)

	sort.Sort(Reverse(sort.IntSlice(lst)))
	fmt.Println(lst)
}

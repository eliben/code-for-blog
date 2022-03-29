// Simple bubbleUp function for disassembly.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"fmt"
	"sort"
)

// bubbleUp is one pass of the bubble sort algorithm. It will bubble up the
// largest element in x to the last position, and may reorder some other
// elements.
func bubbleUp(x sort.Interface) {
	n := x.Len()
	for i := 1; i < n; i++ {
		if x.Less(i, i-1) {
			x.Swap(i, i-1)
		}
	}
}

func main() {
	is := []int{4, 2, 8, 3, 2, 1}
	bubbleUp(sort.IntSlice(is))
	fmt.Println(is)
}

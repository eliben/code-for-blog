package main

import (
	"fmt"

	"github.com/google/go-cmp/cmp"
	"github.com/google/go-cmp/cmp/cmpopts"
)

func main() {
	s1 := []int{42, 12, 23, 2}
	s2 := []int{12, 2, 23, 42}

	if cmp.Equal(s1, s2, cmpopts.SortSlices(intLess)) {
		fmt.Println("slices are equal")
	}
}

func intLess(x, y int) bool {
	return x < y
}

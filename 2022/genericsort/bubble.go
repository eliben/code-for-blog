// Implementation of sorting.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"math/rand"
	"sort"
	"strings"

	"golang.org/x/exp/constraints"
)

func bubbleSortGeneric[T constraints.Ordered](x []T) {
	n := len(x)
	for {
		swapped := false
		for i := 1; i < n; i++ {
			if x[i] < x[i-1] {
				x[i-1], x[i] = x[i], x[i-1]
				swapped = true
			}
		}
		if !swapped {
			return
		}
	}
}

func bubbleSortInterface(x sort.Interface) {
	n := x.Len()
	for {
		swapped := false
		for i := 1; i < n; i++ {
			if x.Less(i, i-1) {
				x.Swap(i, i-1)
				swapped = true
			}
		}
		if !swapped {
			return
		}
	}
}

func bubbleSortFunc[T any](x []T, less func(a, b T) bool) {
	n := len(x)
	for {
		swapped := false
		for i := 1; i < n; i++ {
			if less(x[i], x[i-1]) {
				x[i-1], x[i] = x[i], x[i-1]
				swapped = true
			}
		}
		if !swapped {
			return
		}
	}
}

// A variant of bubbleSortFunc where the comparison function uses a type
// parameter.
func bubbleSortFuncGen[T any, LT func(a, b T) bool](x []T, less LT) {
	n := len(x)
	for {
		swapped := false
		for i := 1; i < n; i++ {
			if less(x[i], x[i-1]) {
				x[i-1], x[i] = x[i], x[i-1]
				swapped = true
			}
		}
		if !swapped {
			return
		}
	}
}

type myStruct struct {
	a, b, c, d string
	n          int
}

type myStructs []*myStruct

func (s myStructs) Len() int           { return len(s) }
func (s myStructs) Less(i, j int) bool { return s[i].n < s[j].n }
func (s myStructs) Swap(i, j int)      { s[i], s[j] = s[j], s[i] }

func makeRandomStrings(n int) []string {
	var letters = []rune("abcdefghijklmnopqrstuvwxyz")
	ss := make([]string, n)
	for i := 0; i < n; i++ {
		// Each random string has length between 2-11
		var sb strings.Builder
		slen := 2 + rand.Intn(10)
		for j := 0; j < slen; j++ {
			sb.WriteRune(letters[rand.Intn(len(letters))])
		}
		ss[i] = sb.String()
	}
	return ss
}

func makeRandomStructs(n int) myStructs {
	structs := make([]*myStruct, n)
	for i := 0; i < n; i++ {
		structs[i] = &myStruct{n: rand.Intn(n)}
	}
	return structs
}

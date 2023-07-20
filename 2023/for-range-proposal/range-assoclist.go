// Example: creating an iterator over a custom data structure.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"fmt"
	"strings"
)

type AssocList[K comparable, V any] struct {
	lst []pair[K, V]
}

type pair[K comparable, V any] struct {
	key   K
	value V
}

func (al *AssocList[K, V]) Add(key K, value V) {
	al.lst = append(al.lst, pair[K, V]{key, value})
}

func (al *AssocList[K, V]) All(yield func(K, V) bool) bool {
	for _, p := range al.lst {
		if !yield(p.key, p.value) {
			return false
		}
	}
	return true
}

func main() {
	al := &AssocList[int, string]{}
	al.Add(10, "ten")
	al.Add(20, "twenty")
	al.Add(5, "five")

	fmt.Println(al)

	for k, v := range al.All {
		fmt.Printf("key=%v, value=%v\n", k, v)
	}

	for k, v := range al.All {
		if strings.HasPrefix(v, "fi") {
			fmt.Println("found bad value, aborting!")
			break
		}
		fmt.Printf("key=%v, value=%v\n", k, v)
	}
}

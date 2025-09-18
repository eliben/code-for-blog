package main

import (
	"fmt"
	"hash/fnv"
)

func hash_item(item []byte, n uint32) uint32 {
	h := fnv.New32a()
	h.Write(item)
	return h.Sum32() % n
}

func main() {
	items := []string{"hello", "go", "don't stop me now"}

	var n uint32 = 32
	for _, item := range items {
		fmt.Printf("%v (n=%v): %v\n", item, n, hash_item([]byte(item), n))
	}

	n = 33
	for _, item := range items {
		fmt.Printf("%v (n=%v): %v\n", item, n, hash_item([]byte(item), n))
	}
}

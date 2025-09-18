package main

import "fmt"

func demo1() {
	items := []string{"hello", "go", "don't stop me now"}

	var n uint32 = 32
	for _, item := range items {
		fmt.Printf("%v (n=%v): %v\n", item, n, hashItem([]byte(item), n))
	}

	fmt.Println("")
	n = 33
	for _, item := range items {
		fmt.Printf("%v (n=%v): %v\n", item, n, hashItem([]byte(item), n))
	}
}

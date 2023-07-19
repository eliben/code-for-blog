package main

import "fmt"

func main() {
	for i := range 5 {
		fmt.Println(i)
	}

	n := 4
	for i := range n {
		fmt.Printf("i=%d, n=%d\n", i, n)
	}

	for range n {
		fmt.Println("beep")
	}
}

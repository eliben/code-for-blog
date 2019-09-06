package main

import (
	"fmt"
	"time"
)

func foobyref(n *int) {
	fmt.Println(*n)
}

func main() {
	for i := 0; i < 5; i++ {
		go foobyref(&i)
	}

	time.Sleep(100 * time.Millisecond)
}

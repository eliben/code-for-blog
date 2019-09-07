package main

import (
	"fmt"
	"time"
)

func foobyval(n int) {
	fmt.Println(n)
}

func main() {
	for i := 0; i < 5; i++ {
		go func() {
			foobyval(i)
		}()
	}

	time.Sleep(100 * time.Millisecond)
}

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
		ii := i
		go func() {
			foobyval(ii)
		}()
	}

	time.Sleep(100 * time.Millisecond)
}

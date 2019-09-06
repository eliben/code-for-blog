package main

import (
	"fmt"
	"time"
)

type MyInt int

func (mi *MyInt) Show() {
	fmt.Println(*mi)
}

func main() {
	ms := []MyInt{50, 60, 70, 80, 90}
	for _, m := range ms {
		go m.Show()
	}

	time.Sleep(100 * time.Millisecond)
}

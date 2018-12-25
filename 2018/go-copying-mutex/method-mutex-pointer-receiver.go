// Pointer receiver properly referring to mutex.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"fmt"
	"sync"
	"time"
)

type Container struct {
	sync.Mutex
	counters map[string]int
}

// Here the receiver is by-pointer, so the locking works as expected.
func (c *Container) inc(name string) {
	c.Lock()
	defer c.Unlock()
	c.counters[name]++
}

func main() {
	c := Container{counters: map[string]int{"a": 0, "b": 0}}

	doIncrement := func(name string, n int) {
		for i := 0; i < n; i++ {
			c.inc(name)
		}
	}

	go doIncrement("a", 100000)
	go doIncrement("a", 100000)

	// Wait a bit for the goroutines to finish
	time.Sleep(300 * time.Millisecond)
	fmt.Println(c.counters)
}

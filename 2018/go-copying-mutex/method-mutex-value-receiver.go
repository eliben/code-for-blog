// Value receiver copying mutex.
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

// Whenever inc is called, the value "c" is copied inside. Since maps are just
// references, they end up pointing to the same map.
// https://github.com/golang/go/blob/master/src/runtime/map.go has some details
// on the internal implementation of maps
// Here the embedded mutex is copied too, so the locking isn't really effective.
func (c Container) inc(name string) {
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

	// The Go runtime should detect concurrent map writes and abort the program.
	// When run with -race, more details will be provided.
	go doIncrement("a", 100000)
	go doIncrement("a", 100000)

	// Wait a bit for the goroutines to finish
	time.Sleep(300 * time.Millisecond)
	fmt.Println(c.counters)
}

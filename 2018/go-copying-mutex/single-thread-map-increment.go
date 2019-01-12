// Base example where a single goroutine increments map contents using a value
// receiver.
//
// This code works but isn't good style - a pointer receiver should be used in
// the inc() method instead.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
package main

import "fmt"

type Container struct {
	counters map[string]int
}

func (c Container) inc(name string) {
	c.counters[name]++
}

func main() {
	c := Container{counters: map[string]int{"a": 0, "b": 0}}

	doIncrement := func(name string, n int) {
		for i := 0; i < n; i++ {
			c.inc(name)
		}
	}

	doIncrement("a", 100000)

	fmt.Println(c.counters)
}

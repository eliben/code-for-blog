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
			c.inc("a")
		}
	}

	doIncrement("a", 100000)

	fmt.Println(c.counters)
}

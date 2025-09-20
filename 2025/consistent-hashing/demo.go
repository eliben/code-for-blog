package main

import (
	"fmt"
	"math/rand/v2"
)

func demo1() {
	items := []string{"hello", "go", "don't stop me now"}

	var n uint64 = 32
	for _, item := range items {
		fmt.Printf("%v (n=%v): %v\n", item, n, hashItem(item, n))
	}

	fmt.Println("")
	n = 33
	for _, item := range items {
		fmt.Printf("%v (n=%v): %v\n", item, n, hashItem(item, n))
	}
}

func dumpDistribution() {
	s1, s2 := rand.Uint64(), rand.Uint64()
	rnd := rand.New(rand.NewPCG(s1, s2))
	ch := NewConsistentHasher(1024 * 1024)

	var nodes []string
	for i := range 16 {
		n := fmt.Sprintf("n%03d", i)
		nodes = append(nodes, n)
		if err := ch.AddNode(n); err != nil {
			panic(err)
		}
	}

	cnt := make(map[string]int)
	for range 1000 {
		str := generateRandomString(rnd, 16)
		nn := ch.FindNodeFor(str)
		cnt[nn] += 1
	}

	for n, c := range cnt {
		fmt.Println(n, c)
	}
}

func main() {
	dumpDistribution()
}

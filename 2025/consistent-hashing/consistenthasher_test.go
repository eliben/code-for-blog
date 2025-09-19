package main

import (
	"fmt"
	"log"
	"math/rand/v2"
	"testing"
)

func makeLoggedRand(t *testing.T) *rand.Rand {
	s1, s2 := rand.Uint64(), rand.Uint64()
	log.Printf("%s seed: %v, %v", t.Name(), s1, s2)
	return rand.New(rand.NewPCG(s1, s2))
}

const charset = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

func generateRandomString(rnd *rand.Rand, length int) string {
	b := make([]byte, length)
	for i := range b {
		b[i] = charset[rnd.IntN(len(charset))]
	}
	return string(b)
}

func TestConsistentHasher(t *testing.T) {
	rnd := makeLoggedRand(t)
	ch := NewConsistentHasher(16 * 1024)

	// Add nodes named "node-N"
	var nodes []string
	for i := range 8 {
		n := fmt.Sprintf("node-%03d", i)
		nodes = append(nodes, n)
		if err := ch.AddNode(n); err != nil {
			t.Error(err)
		}
	}

	fmt.Println(ch)

	for range 10 {
		str := generateRandomString(rnd, 16)
		sh := hashItem(str, ch.ringSize)
		fmt.Printf("hash = %v\n", sh)

		nn := ch.FindNodeFor(str)

		// Now find the closest (from above) node in the ConsistentHasher,
		// and verify that's what we got.
		bestDiff := ch.ringSize
		bestSlot := 0
		for i, s := range ch.slots {
			if s >= sh && (s-sh) < bestDiff {
				bestDiff = s - sh
				bestSlot = i
			}
		}

		// Special case if wrap around
		if sh > ch.slots[len(ch.slots)-1] {
			diff := ch.ringSize - sh + ch.slots[0]
			if diff < bestDiff {
				bestDiff = diff
				bestSlot = 0
			}
		}

		fmt.Printf("%v  ---  %v\n", nn, ch.nodes[bestSlot])
	}

	//fmt.Println(counts)
	//for k, v := range counts {
	//fmt.Printf("  %v: %v\n", k, v)
	//}
}

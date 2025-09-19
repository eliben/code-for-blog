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
	ch := NewConsistentHasher(1024 * 1024)

	// Add nodes named "node-N"
	var nodes []string
	for i := range 16 {
		n := fmt.Sprintf("node-%03d", i)
		nodes = append(nodes, n)
		if err := ch.AddNode(n); err != nil {
			t.Error(err)
		}
	}

	counts := make(map[string]int)
	for range 100000 {
		s := generateRandomString(rnd, 16)
		nn := ch.FindNodeFor(s)
		counts[nn] += 1
	}

	fmt.Println(counts)
}

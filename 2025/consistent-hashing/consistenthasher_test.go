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

func TestSmoke(t *testing.T) {
	ch := NewConsistentHasher(1024)

	// Add nodes named "node-N"
	var nodes []string
	for i := range 8 {
		n := fmt.Sprintf("node-%03d", i)
		nodes = append(nodes, n)
		if err := ch.AddNode(n); err != nil {
			t.Error(err)
		}
	}

	s := "blobs"
	ch.FindNodeFor(s)
}

func TestFindsCorrectNode(t *testing.T) {
	rnd := makeLoggedRand(t)
	ch := NewConsistentHasher(1024 * 1024)

	// Add nodes named "node-N"
	var nodes []string
	for i := range 256 {
		n := fmt.Sprintf("node-%03d", i)
		nodes = append(nodes, n)
		if err := ch.AddNode(n); err != nil {
			t.Error(err)
		}
	}

	// Repeat many times:
	// - generate a random item
	// - ask the ConsistentHasher which node it hashes to
	// - run a manual search process using ConsistentHasher's internals to verify
	//   that the item was hashed to the right node
	for range 1000 {
		str := generateRandomString(rnd, 16)
		sh := hashItem(str, ch.ringSize)

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

		if nn != ch.nodes[bestSlot] {
			t.Errorf("mismatch; ch returned %v, manual search %v", nn, ch.nodes[bestSlot])
		}
	}
}

func TestConsistentAfterAdd(t *testing.T) {
	rnd := makeLoggedRand(t)
	ch := NewConsistentHasher(1024 * 1024)

	// Add nodes named "node-N"
	var nodes []string
	for i := range 32 {
		n := fmt.Sprintf("node-%03d", i)
		nodes = append(nodes, n)
		if err := ch.AddNode(n); err != nil {
			t.Error(err)
		}
	}

	// Generate random items and write down which nodes they hashed to
	mapBeforeAdd := make(map[string]string)
	for range 100000 {
		str := generateRandomString(rnd, 16)
		nn := ch.FindNodeFor(str)
		mapBeforeAdd[str] = nn
	}

	// Now add a new node.
	newNode := "anewnode"
	if err := ch.AddNode(newNode); err != nil {
		t.Error(err)
	}

	// Hash the same items again; the node they are hashed to shouldn't have
	// changed, unless it's to newNode. Also, expect that at least some items
	// were rehashed.
	rehashedCount := 0
	for item, n := range mapBeforeAdd {
		nn := ch.FindNodeFor(item)
		if nn != n {
			if nn != newNode {
				t.Errorf("%v rehashed from %v to %v", item, n, nn)
			}
			rehashedCount += 1
		}
	}
	if rehashedCount == 0 {
		t.Errorf("got rehashedCount=0")
	}
}

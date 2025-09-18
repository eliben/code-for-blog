package main

import (
	"hash/fnv"
	"slices"
)

// hashItem computes the bucket an item hashes to, given a total number of
// buckets.
func hashItem(item string, nbuckets uint32) uint32 {
	h := fnv.New32a()
	h.Write([]byte(item))
	return h.Sum32() % nbuckets
}

type ConsistentHasher struct {
	nodes []string

	// slots is a sorted slice of node indices.
	slots []uint32

	ringSize uint32
}

func NewConsistentHasher(ringSize uint32) *ConsistentHasher {
	return &ConsistentHasher{
		ringSize: ringSize,
	}
}

func (ch *ConsistentHasher) FindNodeFor(item string) string {
	ih := hashItem(item, ch.ringSize)

	// Since ch.slots is a sorted list of all the slot indices for our nodes, a
	// binary search is what we need here. ih is mapped to the node that has the
	// same or the next larger slot index. slices.BinarySearch does exactly this,
	// by returning the index where the value would be inserted.
	//
	// When the returned index is >= len(slots), it means the search wrapped
	// around and should be inserted at 0.
	slotIndex, _ := slices.BinarySearch(ch.slots, ih)
	if slotIndex >= len(ch.slots) {
		slotIndex = 0
	}

	return ch.nodes[slotIndex]
}

func main() {
	demo1()
}

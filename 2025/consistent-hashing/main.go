package main

import (
	"fmt"
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

// TODO: note, only call this if there are nodes already
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

func (ch *ConsistentHasher) AddNode(node string) error {
	if len(ch.nodes) == int(ch.ringSize) {
		return fmt.Errorf("ringSize (%v) exceeded", ch.ringSize)
	}

	// Hash the new node and find where its index fits in the existing ring of
	// slots. If there's a collision, report an error.
	nh := hashItem(node, ch.ringSize)
	slotIndex, found := slices.BinarySearch(ch.slots, nh)

	if found {
		return fmt.Errorf("collision: node %v maps to the same slot (%v)", ch.nodes[slotIndex], nh)
	}

	// No collision, so slotIndex points to the index in ch.nodes where the node
	// should be inserted.
	ch.slots = slices.Insert(ch.slots, slotIndex, nh)
	ch.nodes = slices.Insert(ch.nodes, slotIndex, node)
	return nil
}

func main() {
	demo1()
}

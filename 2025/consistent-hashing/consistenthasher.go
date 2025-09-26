// Consistent hashing implementation in Go.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"crypto/md5"
	"encoding/binary"
	"fmt"
	"slices"
)

// hashItem computes the slot an item hashes to, given a total number of
// slots.
func hashItem(item string, nslots uint64) uint64 {
	digest := md5.Sum([]byte(item))
	digestHigh := binary.BigEndian.Uint64(digest[8:16])
	digestLow := binary.BigEndian.Uint64(digest[:8])
	return (digestHigh | digestLow) % nslots
}

type ConsistentHasher struct {
	// nodes is a list of nodes in the hash ring; it's sorted in the same order
	// as slots: for each i, the node at index slots[i] is nodes[i].
	nodes []string

	// slots is a sorted slice of node indices.
	slots []uint64

	ringSize uint64
}

// NewConsistentHasher creates a new consistent hasher with a given maximal
// ring size.
func NewConsistentHasher(ringSize uint64) *ConsistentHasher {
	return &ConsistentHasher{
		ringSize: ringSize,
	}
}

// FindNodeFor finds the node an item hashes to. It's an error to call this
// method if the hasher doesn't have any nodes.
func (ch *ConsistentHasher) FindNodeFor(item string) string {
	if len(ch.nodes) == 0 {
		panic("FindNodeFor called when ConsistentHasher has no nodes")
	}
	ih := hashItem(item, ch.ringSize)

	// Since ch.slots is a sorted list of all the node indices for our nodes, a
	// binary search is what we need here. ih is mapped to the node that has the
	// same or the next larger node index. slices.BinarySearch does exactly this,
	// by returning the index where the value would be inserted.
	slotIndex, _ := slices.BinarySearch(ch.slots, ih)

	// When the returned index is len(slots), it means the search wrapped
	// around.
	if slotIndex == len(ch.slots) {
		slotIndex = 0
	}

	return ch.nodes[slotIndex]
}

// AddNode adds a node to the hasher.
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

// RemoveNode removes a node from the hasher.
func (ch *ConsistentHasher) RemoveNode(node string) error {
	// Find the node we want to remove. We expect to find it in the list.
	nh := hashItem(node, ch.ringSize)
	slotIndex, found := slices.BinarySearch(ch.slots, nh)
	if !found {
		return fmt.Errorf("removing node %v that doesn't exist", node)
	}

	ch.slots = slices.Delete(ch.slots, slotIndex, slotIndex+1)
	ch.nodes = slices.Delete(ch.nodes, slotIndex, slotIndex+1)
	return nil
}

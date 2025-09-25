package main

import (
	"fmt"
	"slices"
	"strings"
)

type ConsistentHasherV struct {
	// nodes is a list of nodes in the hash ring; it's sorted in the same order
	// as slots: for each i, the node at index slots[i] is nodes[i].
	nodes []string

	// slots is a sorted slice of node indices.
	slots []uint64

	ringSize uint64

	vnodesPerNode int
}

// NewConsistentHasherV creates a new consistent hasher with a given maximal
// ring size. A default of 10 vnodesPerNode is used.
func NewConsistentHasherV(ringSize uint64) *ConsistentHasherV {
	return &ConsistentHasherV{
		ringSize:      ringSize,
		vnodesPerNode: 10,
	}
}

func (ch *ConsistentHasherV) FindNodeFor(item string) string {
	if len(ch.nodes) == 0 {
		panic("FindNodeFor called when ConsistentHasher has no nodes")
	}
	ih := hashItem(item, ch.ringSize)

	slotIndex, _ := slices.BinarySearch(ch.slots, ih)

	if slotIndex == len(ch.slots) {
		slotIndex = 0
	}

	return nodeFromVnode(ch.nodes[slotIndex])
}

func nodeFromVnode(vnodeName string) string {
	node, _, found := strings.Cut(vnodeName, "@")
	if !found {
		panic(fmt.Sprintf("invalid vnode name %v", vnodeName))
	}
	return node
}

func vnodeForNode(nodeName string, i int) string {
	return fmt.Sprintf("%v@%v", nodeName, i)
}

func (ch *ConsistentHasherV) AddNode(node string) error {
	if len(ch.nodes) >= int(ch.ringSize)-ch.vnodesPerNode {
		return fmt.Errorf("ringSize (%v) exceeded", ch.ringSize)
	}

	// Generate vnodes for this node, and insert each one.
	for i := range ch.vnodesPerNode {
		vnode := vnodeForNode(node, i)

		nh := hashItem(vnode, ch.ringSize)
		slotIndex, found := slices.BinarySearch(ch.slots, nh)

		if found {
			return fmt.Errorf("collision: vnode %v maps to the same slot (%v)", ch.nodes[slotIndex], nh)
		}

		ch.slots = slices.Insert(ch.slots, slotIndex, nh)
		ch.nodes = slices.Insert(ch.nodes, slotIndex, vnode)
	}
	return nil
}

func (ch *ConsistentHasherV) RemoveNode(node string) error {
	for i := range ch.vnodesPerNode {
		vnode := vnodeForNode(node, i)

		nh := hashItem(vnode, ch.ringSize)
		slotIndex, found := slices.BinarySearch(ch.slots, nh)
		if !found {
			return fmt.Errorf("removing vnode %v that doesn't exist", vnode)
		}

		ch.slots = slices.Delete(ch.slots, slotIndex, slotIndex+1)
		ch.nodes = slices.Delete(ch.nodes, slotIndex, slotIndex+1)
	}
	return nil
}

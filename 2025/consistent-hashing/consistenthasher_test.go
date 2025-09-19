package main

import (
	"fmt"
	"testing"
)

func TestConsistentHasher(t *testing.T) {
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
}

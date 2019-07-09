package main

import "testing"

func TestTraverseBasic(t *testing.T) {
	var start int
	var end int

	cb := &GoCallbacks{
		startCb: func(i int) { start = i },
		endCb:   func(a, b int) { end = a + b },
	}
	GoTraverse(cb)

	if start != 100 {
		t.Errorf("start got %v, want %v", start, 100)
	}
	if end != 5 {
		t.Errorf("end got %v, want %v", end, 5)
	}
}

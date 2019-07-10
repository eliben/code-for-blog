package main

import "testing"

type testVisitor struct {
	start int
	end   int
}

func (v *testVisitor) Start(i int) {
	v.start = i
}

func (v *testVisitor) End(a, b int) {
	v.end = a + b
}

func TestTraverseBasic(t *testing.T) {
	var v testVisitor
	GoTraverse("joe", &v)

	if v.start != 100 {
		t.Errorf("start got %v, want %v", v.start, 100)
	}
	if v.end != 5 {
		t.Errorf("end got %v, want %v", v.end, 5)
	}
}

// Run these tests with -race
package main

import (
	"math/rand"
	"sync"
	"testing"
	"time"
)

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

type testVisitorDelay struct {
	start int
	end   int
}

func (v *testVisitorDelay) Start(i int) {
	time.Sleep(time.Duration(1+rand.Intn(5)) * time.Millisecond)
	v.start = i
}

func (v *testVisitorDelay) End(a, b int) {
	time.Sleep(time.Duration(1+rand.Intn(5)) * time.Millisecond)
	v.end = a + b
}

func TestConcurrent(t *testing.T) {
	var wg sync.WaitGroup

	worker := func(i int) {
		var v testVisitorDelay
		GoTraverse("foo", &v)
		if v.start != 100 {
			t.Errorf("start got %v, want %v", v.start, 100)
		}
		if v.end != 5 {
			t.Errorf("end got %v, want %v", v.end, 5)
		}
		wg.Done()
	}

	for i := 0; i < 200; i++ {
		wg.Add(1)
		go worker(i)
	}

	wg.Wait()
}

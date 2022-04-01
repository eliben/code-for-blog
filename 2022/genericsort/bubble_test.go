// Tests and benchmarks.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"fmt"
	"sort"
	"testing"

	"golang.org/x/exp/slices"
)

func TestBubbleSort(t *testing.T) {
	for _, length := range []int{1, 2, 4, 6, 17, 32, 800} {
		testname := fmt.Sprintf("sort-len-%d", length)
		t.Run(testname, func(t *testing.T) {
			// Test that our bubble sort works by comparing it to the built-in sort.
			ss := makeRandomStrings(length)
			ss2 := slices.Clone(ss)
			ss3 := slices.Clone(ss)
			ss4 := slices.Clone(ss)

			sort.Strings(ss)
			bubbleSortInterface(sort.StringSlice(ss2))
			bubbleSortGeneric(ss3)
			bubbleSortFunc(ss4, func(a, b string) bool { return a < b })

			for i := range ss {
				if ss[i] != ss2[i] {
					t.Fatalf("strings mismatch at %d; %s != %s", i, ss[i], ss2[i])
				}
				if ss[i] != ss3[i] {
					t.Fatalf("generic mismatch at %d; %s != %s", i, ss[i], ss3[i])
				}
				if ss[i] != ss4[i] {
					t.Fatalf("generic mismatch at %d; %s != %s", i, ss[i], ss4[i])
				}
			}
		})
	}
}

const N = 1_000

func BenchmarkSortStringInterface(b *testing.B) {
	for i := 0; i < b.N; i++ {
		b.StopTimer()
		ss := makeRandomStrings(N)
		b.StartTimer()
		bubbleSortInterface(sort.StringSlice(ss))
	}
}

func BenchmarkSortStringGeneric(b *testing.B) {
	for i := 0; i < b.N; i++ {
		b.StopTimer()
		ss := makeRandomStrings(N)
		b.StartTimer()
		bubbleSortGeneric(ss)
	}
}

func BenchmarkSortStringFunc(b *testing.B) {
	lessFunc := func(a, b string) bool { return a < b }
	for i := 0; i < b.N; i++ {
		b.StopTimer()
		ss := makeRandomStrings(N)
		b.StartTimer()
		bubbleSortFunc(ss, lessFunc)
	}
}

func TestStructSorts(t *testing.T) {
	ss := makeRandomStructs(200)
	ss2 := make([]*myStruct, len(ss))
	for i := range ss {
		ss2[i] = &myStruct{n: ss[i].n}
	}

	bubbleSortInterface(myStructs(ss))
	bubbleSortFunc(ss2, func(a, b *myStruct) bool { return a.n < b.n })

	for i := range ss {
		if *ss[i] != *ss2[i] {
			t.Fatalf("sortfunc mismatch at %d; %v != %v", i, *ss[i], *ss2[i])
		}
	}
}

func BenchmarkSortStructs(b *testing.B) {
	for i := 0; i < b.N; i++ {
		b.StopTimer()
		ss := makeRandomStructs(N)
		b.StartTimer()
		bubbleSortInterface(myStructs(ss))
	}
}

func BenchmarkSortFuncStructs(b *testing.B) {
	lessFunc := func(a, b *myStruct) bool { return a.n < b.n }
	for i := 0; i < b.N; i++ {
		b.StopTimer()
		ss := makeRandomStructs(N)
		b.StartTimer()
		bubbleSortFunc(ss, lessFunc)
	}
}

package main

import (
	"math/rand"
	"slices"
	"testing"
)

func makeRandomInts(n int) []int {
	ints := make([]int, n)
	for i := 0; i < n; i++ {
		ints[i] = rand.Intn(n)
	}
	return ints
}

const N = 100_000

// Wrong because the benchmark loop keeps "sorting" an already-sorted slice
func BenchmarkSortIntsWrong(b *testing.B) {
	ints := makeRandomInts(N)
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		slices.Sort(ints)
	}
}

func BenchmarkSortInts(b *testing.B) {
	for i := 0; i < b.N; i++ {
		b.StopTimer()
		ints := makeRandomInts(N)
		b.StartTimer()
		slices.Sort(ints)
	}
}

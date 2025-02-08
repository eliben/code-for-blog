// Before Go 1.24 is released, use `gotip` to run this (it uses the new
// B.Loop() functionality introduced in Go 1.24)
package main

import (
	"math"
	"math/rand/v2"
	"testing"
)

func makeRandSlice(sz int) []float32 {
	s := make([]float32, sz)
	for i := range sz {
		s[i] = rand.Float32()
	}
	return s
}

func cosSimGo(a, b []float32) float32 {
	var dot, na, nb float32
	for i, x := range a {
		y := b[i]
		dot += x * y
		na += x * x
		nb += y * y
	}
	return dot / float32(math.Sqrt(float64(na*nb)))
}

const benchArrSize = 1 * 1024 * 1024

func BenchmarkCosSimGo(b *testing.B) {
	aa := makeRandSlice(benchArrSize)
	bb := makeRandSlice(benchArrSize)

	for b.Loop() {
		cosSimGo(aa, bb)
	}
}

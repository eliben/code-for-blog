// Before Go 1.24 is released, use `gotip` to run this (it uses the new
// B.Loop() functionality introduced in Go 1.24)
package main

import (
	"testing"
)

func dotProduct(a, b []float32) float32 {
	var dot float32
	for i := range a {
		dot += a[i] * b[i]
	}
	return dot
}

const benchArrSize = 1024 * 1024

func BenchmarkDot(b *testing.B) {
	aa := make([]float32, benchArrSize)
	bb := make([]float32, benchArrSize)

	for b.Loop() {
		dotProduct(aa, bb)
	}
}

func TestDotProduct(t *testing.T) {
	a := []float32{1.0, 3.0, -2.0}
	b := []float32{5.0, -2.0, -7.0}

	want := float32(13.0)
	if got := dotProduct(a, b); got != want {
		t.Errorf("got %v, want %v", got, want)
	}
}

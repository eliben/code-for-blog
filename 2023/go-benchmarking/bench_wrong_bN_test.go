package main

import (
	"crypto/rand"
	"fmt"
	"testing"
)

// Don't run this benchmark, it gets stuck
func BenchmarkRandPrime(b *testing.B) {
	fmt.Println(b.N)
	for i := 0; i < b.N; i++ {
		rand.Prime(rand.Reader, i)
	}
	fmt.Println("... elapsed:", b.Elapsed())
}

// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"runtime"
	"slices"
	"testing"
)

func TestGenPrimes(t *testing.T) {
	primes20 := []int{2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71}

	var got []int
	for p := range genPrimes {
		got = append(got, p)
		if len(got) >= 20 {
			break
		}
	}

	var gotOpt []int
	for p := range genPrimesOpt {
		gotOpt = append(gotOpt, p)
		if len(gotOpt) >= 20 {
			break
		}
	}

	if !slices.Equal(got, primes20) {
		t.Errorf("got %v, want %v", got, primes20)
	}
	if !slices.Equal(gotOpt, primes20) {
		t.Errorf("opt got %v, want %v", gotOpt, primes20)
	}
}

func BenchmarkGenPrimes(b *testing.B) {
	result := 0
	for i := 0; i < b.N; i++ {
		n := 0
		for p := range genPrimes {
			result += p
			n++
			if n >= 9000 {
				break
			}
		}
	}
	runtime.KeepAlive(result)
}

func BenchmarkGenPrimesOpt(b *testing.B) {
	result := 0
	for i := 0; i < b.N; i++ {
		n := 0
		for p := range genPrimesOpt {
			result += p
			n++
			if n >= 9000 {
				break
			}
		}
	}
	runtime.KeepAlive(result)
}

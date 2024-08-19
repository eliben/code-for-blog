// Go implementation of the unbounded prime sieve, using the
// experimental "range over func" proposal.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"fmt"
	"iter"
)

// genPrimes is an iterator, yielding prime numbers until stopped.
func genPrimes() iter.Seq[int] {
	return func(yield func(int) bool) {
		// maps composite numbers to primes that factor them
		d := make(map[int][]int)

		for q := 2; ; q += 1 {
			if factors, found := d[q]; found {
				// q is composite and d[q] is the list of its prime factors; we don't
				// need q in the map any more; mark the next multiples of its factors
				// to prepare for the future
				for _, p := range factors {
					d[p+q] = append(d[p+q], p)
				}
				delete(d, q)
			} else {
				// q is not in the composites map, so it's prime!
				if !yield(q) {
					return
				}
				// Mark q's fist multiple that's not already in d as composite
				d[q*q] = []int{q}
			}

		}
	}
}

func genPrimesOpt() iter.Seq[int] {
	return func(yield func(int) bool) {
		if !yield(2) {
			return
		}
		d := make(map[int]int)

		for q := 3; ; q += 2 {
			if p, hasP := d[q]; hasP {
				delete(d, q)
				x := q + p + p
				for d[x] != 0 {
					x += p + p
				}
				d[x] = p
			} else {
				d[q*q] = q
				if !yield(q) {
					return
				}
			}
		}
	}
}

func main() {
	for p := range genPrimes() {
		fmt.Println(p)

		if p > 100 {
			break
		}
	}
}

package main

import "fmt"

func main() {
	for p := range genPrimes {
		fmt.Println(p)

		if p > 100 {
			break
		}
	}
}

func genPrimes(yield func(int) bool) bool {
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
				return false
			}
			// Mark q's fist multiple that's not already in d as composite
			d[q*q] = []int{q}
		}

	}

	return true
}

func genPrimesOpt(yield func(int) bool) bool {
	if !yield(2) {
		return false
	}

	d := make(map[int]int)

	for q := 3; ; q += 2 {
		if p, hasP := d[q]; !hasP {
			d[q*q] = q
			if !yield(q) {
				return false
			}
		} else {
			delete(d, q)
			x := q + p + p
			for d[x] != 0 {
				x += p + p
			}
			d[x] = p
		}
	}
	return true
}

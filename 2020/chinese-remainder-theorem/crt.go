// Solver for the Chinese Remainder Theorem (CRT).
//
// All functions are to be used as follows:
//
//   result := f(a, n)
//
// Where 'a' is the slice of residues and 'n' the slice of corresponding moduli.
// These represent the set of congruences:
//
//    x = a[0] (mod n[0])
//    x = a[1] (mod n[1])
//    ...
//
// Restrictions: len(a) == len(n). Also, all n have to be coprime; otherwise
// the result is not guaranteed to be correct.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"fmt"
	"math/big"
)

func crtSearch(a, n []int64) int64 {
	var N int64 = 1
	for _, nk := range n {
		N *= nk
	}

search:
	for i := int64(0); i < N; i++ {
		// Does i satisfy all the congruences?
		for k := 0; k < len(n); k++ {
			if i%n[k] != a[k] {
				continue search
			}
		}
		return i
	}

	return -1
}

func crtSieve(a, n []int64) int64 {
	var N int64 = 1
	for _, nk := range n {
		N *= nk
	}

	base := a[0]
	incr := n[0]

nextBase:
	// This loop goes over the congruences one by one; base is a solution
	// to the congruences seen so far.
	for i := 1; i < len(a); i++ {
		// Find a solution that works for the new congruence a[i] as well.
		for candidate := base; candidate < N; candidate += incr {
			if candidate%n[i] == a[i] {
				base = candidate
				incr *= n[i]
				continue nextBase
			}
		}
		// Inner loop exited without finding candidate
		return -1
	}
	return base
}

func crtSieveBig(a, n []*big.Int) *big.Int {
	// Same algorithm as crtSieve, but converted to use big.Int
	N := new(big.Int).Set(n[0])
	for _, nk := range n[1:] {
		N.Mul(N, nk)
	}

	base := a[0]
	incr := n[0]

nextBase:
	for i := 1; i < len(a); i++ {
		for candidate := base; candidate.Cmp(N) < 0; candidate.Add(candidate, incr) {
			mod := new(big.Int).Mod(candidate, n[i])
			if mod.Cmp(a[i]) == 0 {
				base = candidate
				incr.Mul(incr, n[i])
				continue nextBase
			}
		}
		return big.NewInt(-1)
	}
	return base
}

func crtConstructBig(a, n []*big.Int) *big.Int {
	// Compute N: product(n[...])
	N := new(big.Int).Set(n[0])
	for _, nk := range n[1:] {
		N.Mul(N, nk)
	}

	// x is the accumulated answer.
	x := new(big.Int)

	for k, nk := range n {
		// Nk = N/nk
		Nk := new(big.Int).Div(N, nk)

		// N'k (Nkp) is the multiplicative inverse of Nk modulo nk.
		Nkp := new(big.Int)
		if Nkp.ModInverse(Nk, nk) == nil {
			return big.NewInt(-1)
		}

		// x += ak*Nk*Nkp
		x.Add(x, Nkp.Mul(a[k], Nkp.Mul(Nkp, Nk)))
	}
	return x.Mod(x, N)
}

func main() {
	fmt.Println("run tests")
}

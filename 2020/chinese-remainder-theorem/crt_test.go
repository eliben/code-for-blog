package main

import (
	"fmt"
	"math/big"
	"testing"
)

// bigIntSlice takes a slice of int64 and returns a slice of them all converted
// to *big.Ints
func bigIntSlice(nums []int64) []*big.Int {
	b := make([]*big.Int, len(nums))
	for i, n := range nums {
		b[i] = big.NewInt(n)
	}
	return b
}

func TestCRTs(t *testing.T) {
	var tests = []struct {
		a, n   []int64
		answer int64
	}{
		{[]int64{0, 3, 4}, []int64{3, 4, 5}, 39},
		{[]int64{2, 4, 5}, []int64{3, 5, 7}, 89},
		{[]int64{19, 6, 7, 11}, []int64{31, 32, 99, 23}, 138310},
		{[]int64{2292, 3010, 500, 399}, []int64{77003, 61223, 60161, 25873}, 4412381708627286819},

		{[]int64{0, 3, 4}, []int64{3, 4, 9}, -1},
	}

	for _, tt := range tests {
		ttname := fmt.Sprintf("a=%v, n=%v", tt.a, tt.n)
		t.Run(ttname, func(t *testing.T) {

			// Only run crtSearch on the smallest examples.
			if tt.n[0] < 1000 {
				r1 := crtSearch(tt.a, tt.n)
				if r1 != tt.answer {
					t.Errorf("crtSearch=%v, want=%v", r1, tt.answer)
				}
			}

			r2 := crtSieve(tt.a, tt.n)
			if r2 != tt.answer {
				t.Errorf("crtSieve=%v, want=%v", r2, tt.answer)
			}

			r3big := crtSieveBig(bigIntSlice(tt.a), bigIntSlice(tt.n))
			if r3big.Cmp(big.NewInt(tt.answer)) != 0 {
				t.Errorf("crtSieveBig=%v, want %v", r3big, tt.answer)
			}

			r4big := crtConstructBig(bigIntSlice(tt.a), bigIntSlice(tt.n))
			if r4big.Cmp(big.NewInt(tt.answer)) != 0 {
				t.Errorf("crtConstructBig=%v, want %v", r4big, tt.answer)
			}
		})
	}
}

func TestCRTHuge(t *testing.T) {
	// Check that both methods return the same result.
	a := []int64{990101, 2019304, 80000, 102, 201, 3010, 500, 399}
	n := []int64{3333383, 2666141, 902761, 668821, 77003, 61223, 60161, 25873}

	r1 := crtSieveBig(bigIntSlice(a), bigIntSlice(n))
	r2 := crtConstructBig(bigIntSlice(a), bigIntSlice(n))

	if r1.Cmp(r2) != 0 {
		t.Errorf("want equal results; got r1=%v, r2=%v", r1, r2)
	}
}

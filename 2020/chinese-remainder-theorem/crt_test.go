package main

import (
	"fmt"
	"math/big"
	"testing"
)

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
		// TODO look for some -1s too
		{[]int64{0, 3, 4}, []int64{3, 4, 5}, 39},
	}

	for _, tt := range tests {
		ttname := fmt.Sprintf("a=%v, n=%v", tt.a, tt.n)
		t.Run(ttname, func(t *testing.T) {
			r1 := crtSearch(tt.a, tt.n)
			if r1 != tt.answer {
				t.Errorf("crtSearch=%v, want=%v", r1, tt.answer)
			}
		})
	}
}

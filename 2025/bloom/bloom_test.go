package bloom

import (
	"fmt"
	"hash/maphash"
	"slices"
	"testing"
)

func TestCalculateParams(t *testing.T) {
	// Estimations from the blog post with 100,000 elements and an error rate
	// of 0.01
	m, k := CalculateParams(100000, 0.01)
	if m < 958000 || m > 959000 {
		t.Errorf("got m=%v", m)
	}
	if k != 7 {
		t.Errorf("got k=%v", k)
	}
}

func TestNewBitset(t *testing.T) {
	// Test length of new bitset
	var tests = []struct {
		m       uint64
		wantLen int
	}{
		{1, 1},
		{20, 1},
		{64, 1},
		{65, 2},
		{66, 2},
		{640, 10},
		{641, 11},
	}

	for _, tt := range tests {
		t.Run(fmt.Sprintf("%v", tt.m), func(t *testing.T) {
			bs := newBitset(tt.m)
			if len(bs) != tt.wantLen {
				t.Errorf("got %v, want %v", len(bs), tt.wantLen)
			}
		})
	}
}

func TestBitsetSetTest(t *testing.T) {
	b := newBitset(68)
	oneIndices := []uint64{1, 20, 33, 61, 67}

	for _, idx := range oneIndices {
		bitsetSet(b, idx)
	}

	for idx := range uint64(68) {
		want := slices.Contains(oneIndices, idx)
		got := bitsetTest(b, idx)
		if got != want {
			t.Errorf("idx=%v got %v, want %v", idx, got, want)
		}
	}
}

func TestMaphash(t *testing.T) {
	s1 := maphash.MakeSeed()

	data := []byte{10, 20, 30}
	b1 := maphash.Bytes(s1, data)

	var hcopy maphash.Hash
	hcopy.SetSeed(s1)
	hcopy.Write(data)
	b11 := hcopy.Sum64()

	// Equal seeds ==> equal hashes for the same data
	if b1 != b11 {
		t.Errorf("got b1=%v, b11=%v", b1, b11)
	}

	// Different seeds ==> different hashes (unless there's a collision, which
	// should be extremely rare)
	s2 := maphash.MakeSeed()
	b2 := maphash.Bytes(s2, data)
	if b1 == b2 {
		t.Errorf("got equality at %v", b1)
	}
}

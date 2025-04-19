package bloom

import (
	"fmt"
	"hash/maphash"
	"testing"
)

func TestNewBitset(t *testing.T) {
	// Test length of new bitset
	var tests = []struct {
		m       uint
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

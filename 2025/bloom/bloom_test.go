package bloom

import (
	"crypto/rand"
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

func TestCalculateEps(t *testing.T) {
	eps := CalculateEps(1000000, 100000)
	if eps < 0.008 || eps > 0.0082 {
		t.Errorf("got eps=%v", eps)
	}
}

func TestFilter(t *testing.T) {
	// Test the operation of the Bloom filter. We set parameters to get
	// an extremely low error rate, and check that we don't get any false
	// answers.
	m := uint64(2000)
	n, k := CalculateParams(m, 1e-15)

	bf := New(n, k)

	// Insert m random items, also holding them in fullSet.
	// We assume that the chance of generating the same 256-byte random
	// value is negligible.
	buf := make([]byte, 256)
	fullSet := make(map[string]bool)
	for range m {
		rand.Read(buf)
		bf.Insert(buf)
		fullSet[string(buf)] = true
	}

	// Check that true is returned for all inserted items (this can never be
	// false due to the guarantees of the Bloom filter)
	for k := range fullSet {
		if !bf.Test([]byte(k)) {
			t.Errorf("Test(%v)=false", k)
		}
	}

	// Now generate another 2000 random items; the chance of false positives is
	// so low that we expect Test to return true for all of these.
	for range m {
		rand.Read(buf)
		if bf.Test(buf) {
			t.Errorf("Test(%v)=true", buf)
		}
	}
}

func TestErrorRate(t *testing.T) {
	// Test the error rate we get from a filter matches theoretical estimates.
	m := uint64(1000)
	eps := 0.1
	n, k := CalculateParams(m, eps)

	bf := New(n, k)
	buf := make([]byte, 256)
	fullSet := make(map[string]bool)
	for range m {
		rand.Read(buf)
		bf.Insert(buf)
		fullSet[string(buf)] = true
	}

	// Check that true is returned for all inserted items (this can never be
	// false due to the guarantees of the Bloom filter)
	for k := range fullSet {
		if !bf.Test([]byte(k)) {
			t.Errorf("Test(%v)=false", k)
		}
	}

	// Now calculate the empirical error rate, by testing a large number of
	// random items (that weren't previously inserted).
	N := 100000
	npos := 0
	for range N {
		rand.Read(buf)
		if bf.Test(buf) {
			npos++
		}
	}

	// Expect the count to be within 25% of our requested eps
	expectedFPs := float64(N) * eps
	nposfp64 := float64(npos)
	if nposfp64 < expectedFPs*0.75 || nposfp64 > expectedFPs*1.25 {
		t.Errorf("got %v, expected %v", nposfp64, expectedFPs)
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

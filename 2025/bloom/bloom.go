package bloom

import (
	"hash/maphash"
	"math"
)

// CalculateParams calculates optimal parameters for a Bloom filter that's
// intended to contain n elements with error (false positive) rate eps.
func CalculateParams(n uint64, eps float64) (m uint64, k uint64) {
	// The formulae we derived are:
	// (m/n) = -ln(eps)/(ln(2)*ln(2))
	// k = (m/n)ln(2)

	ln2 := math.Log(2)
	mdivn := -math.Log(eps) / (ln2 * ln2)
	m = uint64(math.Ceil(float64(n) * mdivn))
	k = uint64(math.Ceil(mdivn * ln2))
	return
}

func New(m uint64, k uint64) *BloomFilter {
	return &BloomFilter{
		m:      m,
		k:      k,
		bitset: newBitset(m),
		seed1:  maphash.MakeSeed(),
		seed2:  maphash.MakeSeed(),
	}
}

type BloomFilter struct {
	m      uint64
	k      uint64
	bitset []uint64

	seed1, seed2 maphash.Seed
}

func (bf *BloomFilter) Add(data []byte) {
	h1 := maphash.Bytes(bf.seed1, data)
	h2 := maphash.Bytes(bf.seed2, data)
	for i := range bf.k {
		loc := (h1 + i*h2) % bf.m
		bitsetSet(bf.bitset, loc)
	}
}

func (bf *BloomFilter) Test(data []byte) bool {
	h1 := maphash.Bytes(bf.seed1, data)
	h2 := maphash.Bytes(bf.seed2, data)
	for i := range bf.k {
		loc := (h1 + i*h2) % bf.m
		if !bitsetTest(bf.bitset, loc) {
			return false
		}
	}
	return true
}

// newBitset creates a new bitset to store m bits.
func newBitset(m uint64) []uint64 {
	blen := (m + 63) / 64
	return make([]uint64, blen)
}

// bitsetSet sets bit i in the given bitset.
func bitsetSet(bitset []uint64, i uint64) {
	word, offset := i/64, i%64
	bitset[word] |= (1 << offset)
}

// bitsetTest tests the value of bit i in the given bitset. Returns true if
// that bit is 1, false otherwise.
func bitsetTest(bitset []uint64, i uint64) bool {
	word, offset := i/64, i%64
	return (bitset[word] & (1 << offset)) != 0
}

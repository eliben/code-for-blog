package bloom

import "hash/maphash"

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

// newBitset creates a new bitset to store m bits.
func newBitset(m uint64) []uint64 {
	blen := (m + 63) / 64
	return make([]uint64, blen)
}

func (bf *BloomFilter) Add(data []byte) {
	for i := range bf.k {
		h1 := maphash.Bytes(bf.seed1, data)
		h2 := maphash.Bytes(bf.seed2, data)
		loc := (h1 + i*h2) % bf.m
		bitsetSet(bf.bitset, loc)
	}
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

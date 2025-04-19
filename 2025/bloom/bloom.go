package bloom

import "hash/maphash"

func New(m uint, k uint) *BloomFilter {
	return &BloomFilter{
		m:      m,
		k:      k,
		bitset: newBitset(m),
		seed1:  maphas.MakeSeed(),
		seed2:  maphas.MakeSeed(),
	}
}

type BloomFilter struct {
	m      uint
	k      uint
	bitset []uint64

	seed1, seed2 maphash.Seed
}

// newBitset creates a new bitset to store m bits.
func newBitset(m uint) []uint64 {
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
func bitsetSet(bitset []uint64, i int) {
	// TODO
}

package main

import (
	"hash/maphash"
	"testing"
)

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

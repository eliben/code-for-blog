package main

import (
	"hash/fnv"
)

// hashItem computes the bucket an item hashes to, given a total number of
// buckets.
func hashItem(item []byte, nbuckets uint32) uint32 {
	h := fnv.New32a()
	h.Write(item)
	return h.Sum32() % nbuckets
}

func main() {
	demo1()
}

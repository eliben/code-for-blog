// Benchmarking samples.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"bytes"
	"fmt"
	"hash/crc32"
	"testing"
)

func BenchmarkBasic(b *testing.B) {
	bs := bytes.Repeat([]byte{2, 3, 5, 7}, 2048)
	fmt.Println("benchmark with N =", b.N)

	for i := 0; i < b.N; i++ {
		crc32.ChecksumIEEE(bs)
	}

	// Prints elapsed time to see how long each invocation of BenchmarkBasic by
	// the harness takes.
	fmt.Println("... elapsed:", b.Elapsed())
}

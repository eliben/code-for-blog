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
	fmt.Println("... elapsed:", b.Elapsed())
}

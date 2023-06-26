package main

import (
	"runtime"
	"testing"
)

func isCond(b byte) bool {
	if b%3 == 1 && b%7 == 2 && b%17 == 11 && b%31 == 9 {
		return true
	}
	return false
}

// Wrong: the whole loop is optimized away
func BenchmarkIsCondWrong(b *testing.B) {
	for i := 0; i < b.N; i++ {
		isCond(201)
	}
}

func countCond(b []byte) int {
	result := 0
	for i := 0; i < len(b); i++ {
		if isCond(b[i]) {
			result++
		}
	}
	return result
}

// Wrong: the contents of countCond are optimized away
func BenchmarkCountWrong(b *testing.B) {
	inp := getInputContents()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		countCond(inp)
	}
}

var Sink int

func BenchmarkCountSink(b *testing.B) {
	inp := getInputContents()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		Sink += countCond(inp)
	}
}

func BenchmarkCountKeepAlive(b *testing.B) {
	inp := getInputContents()
	b.ResetTimer()
	result := 0
	for i := 0; i < b.N; i++ {
		result += countCond(inp)
	}
	runtime.KeepAlive(result)
}

func getInputContents() []byte {
	n := 400000
	buf := make([]byte, n)
	for i := 0; i < n; i++ {
		buf[i] = byte(n % 32)
	}
	return buf
}

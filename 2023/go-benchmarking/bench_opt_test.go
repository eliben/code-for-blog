package main

import "testing"

func isCond(b byte) bool {
	if b%3 == 1 && b%7 == 2 && b%17 == 11 && b%31 == 9 {
		return true
	}
	return false
}

var GlobalVal byte = 201

// Wrong: the whole loop is optimized away
func BenchmarkIsCondWrong(b *testing.B) {
	for i := 0; i < b.N; i++ {
		isCond(GlobalVal)
	}
}

func countCond(b []byte) int {
	result := 0
	for i := 0; i < len(b); i++ {
		if isCond(b[i]) {
			result += 1
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

//go:noinline
func getInputContents() []byte {
	n := 400000
	buf := make([]byte, n)
	for i := 0; i < n; i++ {
		buf[i] = byte(n % 32)
	}
	return buf
}

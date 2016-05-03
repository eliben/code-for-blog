package main

import (
	"bufio"
	"index/suffixarray"
	"log"
	"os"
	"strings"
	"testing"
)

func getDictWords() []string {
	f, err := os.Open(`/usr/share/dict/words`)
	if err != nil {
		log.Fatal(err)
	}

	var words []string
	input := bufio.NewScanner(f)
	input.Split(bufio.ScanLines)
	for input.Scan() {
		words = append(words, strings.ToLower(input.Text()))
	}
	return words
}

func BenchmarkBuildSuffixArray(b *testing.B) {
	words := getDictWords()

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		data := []byte("\x00" + strings.Join(words, "\x00") + "\x00")
		_ = suffixarray.New(data)
	}
}

func BenchmarkSimpleFindMiddle(b *testing.B) {
	words := getDictWords()

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		for _, w := range words {
			if strings.Contains(w, "yrate") {
				break
			}
		}
	}
}

func BenchmarkSimpleFindBeginning(b *testing.B) {
	words := getDictWords()

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		for _, w := range words {
			if strings.Contains(w, "tion") {
				break
			}
		}
	}
}

func BenchmarkLookup(b *testing.B) {
	words := getDictWords()
	data := []byte("\x00" + strings.Join(words, "\x00") + "\x00")
	sa := suffixarray.New(data)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		indices := sa.Lookup([]byte("tion"), 1)
		_ = getStringFromIndex(data, indices[0])
	}
}

func TestDictWords(t *testing.T) {
	words := getDictWords()
	if len(words) < 10000 {
		t.Fatal("too few dict words")
	}
}

func TestSimpleFind(t *testing.T) {
	words := getDictWords()
	var found bool

	for _, w := range words {
		if strings.Contains(w, "tion") {
			found = true
			break
		}
	}

	if !found {
		t.Fatal("not found")
	}
}

func TestSuffixArrayFind(t *testing.T) {
	words := getDictWords()
	data := []byte("\x00" + strings.Join(words, "\x00") + "\x00")
	sa := suffixarray.New(data)

	indices := sa.Lookup([]byte("yrate"), 1)
	if indices == nil || len(indices) < 1 {
		t.Fatal("not found")
	}
}

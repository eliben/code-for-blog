// Go suffixarray sample - benchmarks and basic smoke tests.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"bufio"
	"bytes"
	"fmt"
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

const XXwordToTry = "manipulator"

func BenchmarkSimpleFindMiddleXX(b *testing.B) {
	words := getDictWords()

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		for _, w := range words {
			if strings.Contains(w, XXwordToTry) {
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

func BenchmarkLookupXX(b *testing.B) {
	words := getDictWords()
	data := []byte("\x00" + strings.Join(words, "\x00") + "\x00")
	sa := suffixarray.New(data)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		indices := sa.Lookup([]byte(XXwordToTry), 1)
		if len(indices) > 0 {
			_ = getStringFromIndex(data, indices[0])
		}
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

	for i, w := range words {
		if strings.Contains(w, XXwordToTry) {
			found = true
			fmt.Println(i, w)
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

	buf := &bytes.Buffer{}
	sa.Write(buf)
	fmt.Println("size:", buf.Len())

	indices := sa.Lookup([]byte("yrate"), 1)
	if indices == nil || len(indices) < 1 {
		t.Fatal("not found")
	}
}

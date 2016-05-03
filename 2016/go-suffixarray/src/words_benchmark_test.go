package main

import (
	"bufio"
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

func TestDictWords(t *testing.T) {
	words := getDictWords()
	if len(words) < 110000 {
		t.Fatal("too few dict words")
	}
}

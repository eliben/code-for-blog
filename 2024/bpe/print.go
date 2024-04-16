package bpe

import (
	"fmt"
	"log"
)

func PrintVocab(vocab map[string]int) {
	if len(vocab) < 256 {
		log.Fatalf("got len=%d, want vocab with at least 256 elements", len(vocab))
	}
	fmt.Println("vocab: {")
	fmt.Println("  [0...255]")
	for k, v := range vocab {
		if v >= 256 {
			fmt.Printf("  %q: %d\n", k, v)
		}
	}
	fmt.Println("}")
}

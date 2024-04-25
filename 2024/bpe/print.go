// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
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
	for k, v := range vocab {
		fmt.Printf("  %q: %d\n", k, v)
	}
	fmt.Println("}")
}

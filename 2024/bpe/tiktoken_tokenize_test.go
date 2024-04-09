package bpe

import (
	"fmt"
	"os"
	"path/filepath"
	"testing"
)

const gpt4splitPattern = `(?i:'s|'t|'re|'ve|'m|'ll|'d)|[^\r\n\p{L}\p{N}]?\p{L}+|\p{N}{1,3}| ?[^\s\p{L}\p{N}]+[\r\n]*|\s*[\r\n]+|\s+(?!\S)|\s+`

func TestTiktokenTokenize(t *testing.T) {
	r, err := os.Open(filepath.Join("data", "cl100k_base.tiktoken"))
	if err != nil {
		t.Fatal(err)
	}
	vocab, err := loadTiktokenVocab(r)
	if err != nil {
		t.Fatal(err)
	}

	toks := encode(`Anything is possible!!`, vocab, gpt4splitPattern)
	fmt.Println(toks)
}

package bpe

import (
	"os"
	"path/filepath"
	"strings"
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

	text := "Anything is possible!!"
	toks := encode(text, vocab, gpt4splitPattern)
	if len(toks) != 4 {
		t.Errorf("got len %v, want 4", len(toks))
	}

	d := NewDecoder(vocab)
	parts := d.decode(toks)
	whole := strings.Join(parts, "")
	if whole != text {
		t.Errorf("got whole = %q, not = text", whole)
	}
}

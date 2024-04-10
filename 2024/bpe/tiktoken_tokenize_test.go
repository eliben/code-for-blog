package bpe

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestTiktokenTokenize(t *testing.T) {
	r, err := os.Open(filepath.Join("data", "cl100k_base.tiktoken"))
	if err != nil {
		t.Fatal(err)
	}
	vocab, err := LoadTiktokenVocab(r)
	if err != nil {
		t.Fatal(err)
	}

	text := "Anything is possible!!"
	toks := Encode(text, vocab, CL100KBaseSplitPattern)
	if len(toks) != 4 {
		t.Errorf("got len %v, want 4", len(toks))
	}

	d := NewDecoder(vocab)
	parts := d.Decode(toks)
	whole := strings.Join(parts, "")
	if whole != text {
		t.Errorf("got whole = %q, not = text", whole)
	}

	text2 := "You can use the tool below to understand how a piece of text might be tokenized by a language model, and the total count of tokens in that piece of text."
	toks2 := Encode(text2, vocab, CL100KBaseSplitPattern)
	if len(toks2) != 34 {
		t.Errorf("got len %v, want 34", len(toks2))
	}
}

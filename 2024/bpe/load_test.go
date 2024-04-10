package bpe

import (
	"os"
	"path/filepath"
	"testing"
)

func TestLoadCl100kBase(t *testing.T) {
	r, err := os.Open(filepath.Join("data", "cl100k_base.tiktoken"))
	if err != nil {
		t.Fatal(err)
	}
	vocab, err := LoadTiktokenVocab(r)
	if err != nil {
		t.Fatal(err)
	}

	// Sanity checking that we've loaded it all correctly.
	if len(vocab) != 100256 {
		t.Errorf("got len %v, want 100256", len(vocab))
	}
	if vocab["a"]+1 != vocab["b"] {
		t.Errorf("got %v %v, want consecutive vocab for 'a' and 'b'", vocab["a"], vocab["b"])
	}

	if _, ok := vocab["many"]; !ok {
		t.Errorf("'many' not found in vocab")
	}
}

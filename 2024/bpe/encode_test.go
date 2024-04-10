package bpe

import (
	"testing"
)

func TestEncodeBasic(t *testing.T) {
	txt := "i'm blue dabadee dabadam 999999"

	vocab1 := Train(txt, 256, gpt2splitPattern)
	toks1 := Encode(txt, vocab1, gpt2splitPattern)

	for _, tok := range toks1 {
		if tok >= 256 {
			t.Errorf("found tok=%v, expect none >= 256", tok)
		}
	}

	vocab2 := Train(txt, 258, gpt2splitPattern)
	toks2 := Encode(txt, vocab2, gpt2splitPattern)

	// should have encoded the '99's and 'da'
	tl := len(toks2)
	if vocab2["99"] != 256 || toks2[tl-1] != 256 || toks2[tl-2] != 256 || toks2[tl-3] != 256 {
		t.Errorf("want last three tokens to be 99, got %v", toks2)
	}

	if vocab2["da"] != 257 || toks2[tl-6] != 257 {
		t.Errorf("want token to be 257, got %v", toks2)
	}
}

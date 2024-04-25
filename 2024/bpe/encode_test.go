// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package bpe

import (
	"testing"
)

func TestEncodeBasic(t *testing.T) {
	txt := "i'm blue dabadee dabadam xxxxxx"

	vocab1 := Train(txt, 256, CL100KBaseSplitPattern)
	toks1 := Encode(txt, vocab1, CL100KBaseSplitPattern)

	for _, tok := range toks1 {
		if tok >= 256 {
			t.Errorf("found tok=%v, expect none >= 256", tok)
		}
	}

	vocab2 := Train(txt, 258, CL100KBaseSplitPattern)
	toks2 := Encode(txt, vocab2, CL100KBaseSplitPattern)

	// should have encoded the 'xx's and 'da'
	tl := len(toks2)
	if vocab2["xx"] != 256 || toks2[tl-1] != 256 || toks2[tl-2] != 256 || toks2[tl-3] != 256 {
		t.Errorf("want last three tokens to be 'xx', got %v", toks2)
	}

	if vocab2["da"] != 257 || toks2[tl-6] != 257 {
		t.Errorf("want token to be 257, got %v", toks2)
	}
}

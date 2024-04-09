package bpe

import (
	"testing"
)

const gpt2splitPattern = `'s|'t|'re|'ve|'m|'ll|'d| ?\p{L}+| ?\p{N}+| ?[^\s\p{L}\p{N}]+|\s+(?!\S)|\s+`

func TestTrainBasic(t *testing.T) {
	debugTrain = false // TODO

	txt := "i'm blue dabadee dabadam 999999"
	vocab := train(txt, 258, gpt2splitPattern)

	if len(vocab) != 258 {
		t.Errorf("got len(vocab)=%d, want 258", len(vocab))
	}

	if vocab["99"] != 256 || vocab["da"] != 257 {
		t.Errorf(`vocab["99"]=%v, vocab["da"]=%v`, vocab["99"], vocab["da"])
	}
}

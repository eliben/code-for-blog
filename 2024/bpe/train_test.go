package bpe

import (
	"testing"
)

func TestTrainBasic(t *testing.T) {
	debugTrain = false // TODO

	txt := "i'm blue dabadee dabadam 999999"
	vocab := Train(txt, 258, CL100KBaseSplitPattern)

	if len(vocab) != 258 {
		t.Errorf("got len(vocab)=%d, want 258", len(vocab))
	}

	if vocab["99"] != 256 || vocab["da"] != 257 {
		t.Errorf(`vocab["99"]=%v, vocab["da"]=%v`, vocab["99"], vocab["da"])
	}
}

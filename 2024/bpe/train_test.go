// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package bpe

import (
	"testing"
)

func TestTrainBasic(t *testing.T) {
	debugTrain = true

	txt := "i'm blue dabadee dabadam"
	vocab := Train(txt, 264, CL100KBaseSplitPattern)

	PrintVocab(vocab)
}

func TestTrainBasicWithNums(t *testing.T) {
	debugTrain = false

	txt := "i'm blue dabadee dabadam 999999"
	vocab := Train(txt, 258, CL100KBaseSplitPattern)

	if len(vocab) != 258 {
		t.Errorf("got len(vocab)=%d, want 258", len(vocab))
	}

	if vocab["99"] != 256 || vocab["da"] != 257 {
		t.Errorf(`vocab["99"]=%v, vocab["da"]=%v`, vocab["99"], vocab["da"])
	}

	//PrintVocab(vocab)
}

func TestTrainPlease(t *testing.T) {
	//debugTrain = true
	txt := "snug a bug pleeeeeeeeeease dont be such a pug"
	vocab := Train(txt, 260, CL100KBaseSplitPattern)

	if len(vocab) != 260 {
		t.Errorf("got len(vocab)=%d, want 260", len(vocab))
	}

	if vocab["ee"] != 256 || vocab["eeee"] != 257 || vocab["ug"] != 258 {
		t.Errorf(`vocab["ee"]=%v, vocab["eeee"]=%v, vocab["ug"]=%v`, vocab["ee"], vocab["eeee"], vocab["ug"])
	}

	//PrintVocab(vocab)
}

func TestTrainArtificialForBlogPost(t *testing.T) {
	txt := "abcdede zdede tXOXO vXO"
	vocab := Train(txt, 260, CL100KBaseSplitPattern)

	if vocab["de"] != 256 || vocab["XO"] != 257 || vocab["dede"] != 258 {
		t.Errorf(`vocab["de"]=%v, vocab["XO"]=%v, vocab["dede"]=%v`, vocab["ee"], vocab["eeee"], vocab["ug"])
	}

	PrintVocab(vocab)
}

package bpe

import "testing"

const gpt2splitPattern = `'s|'t|'re|'ve|'m|'ll|'d| ?\p{L}+| ?\p{N}+| ?[^\s\p{L}\p{N}]+|\s+(?!\S)|\s+`

func TestTrainBasic(t *testing.T) {
	txt := "low low low low low lowest lowest newer newer newer newer newer newer wider wider wider new new"
	v := train(txt, 275, gpt2splitPattern)
	_ = v
}

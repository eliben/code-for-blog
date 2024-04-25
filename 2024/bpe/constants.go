// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package bpe

// CL100KBaseSplitPattern is the splitting pattern (regexp) used by tiktoken
// with the cl100k_base vocabulary for GPT 4.
const CL100KBaseSplitPattern = `(?i:'s|'t|'re|'ve|'m|'ll|'d)|[^\r\n\p{L}\p{N}]?\p{L}+|\p{N}{1,3}| ?[^\s\p{L}\p{N}]+[\r\n]*|\s*[\r\n]+|\s+(?!\S)|\s+`

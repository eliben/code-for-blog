// BPE: encoding text using a trained BPE tokenizer.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package bpe

import (
	"fmt"
	"slices"

	"github.com/dlclark/regexp2"
)

const debugEncode = false

// Encode runs a trained BPE tokenizer over the given text. vocab is the learned
// map of tokens (mapping token strings to their numeric IDs). splitPattern is
// the regexp pattern to use for the initial splitting of text to words. Returns
// a list of tokens representing the text.
// Note: expects all tokens in text - including any single byte value - to
// appear in vocab; if an unknown token is found, this function panics.
func Encode(text string, vocab map[string]int, splitPattern string) []int {
	// Split the input into words using the given splitPattern.
	// Each word is split further into a list of single bytes.
	var words [][]string
	re := regexp2.MustCompile(splitPattern, regexp2.None)
	m, _ := re.FindStringMatch(text)
	for m != nil {
		var word []string
		for i := 0; i < len(m.String()); i++ {
			word = append(word, string(m.String()[i]))
		}
		words = append(words, word)
		m, _ = re.FindNextMatch(m)
	}

	if debugEncode {
		fmt.Println("first 20 words before encoding")
		for _, word := range words {
			fmt.Printf("%q\n", word)
		}
	}

	var tokens []int

	// We convert each word in order, adding its tokenized representation to
	// tokens.
	for _, word := range words {
		for {
			// Find a pair of tokens in word that we can combine to a longer token
			// appearing in vocab. BPE requires us to find the earliest token that
			// was merged in the training process (it has the lowest "order" - ID
			// in vocab).
			minIdx := -1
			minVocabOrder := len(vocab) + 1
			for i := 0; i < len(word)-1; i++ {
				if order, ok := vocab[word[i]+word[i+1]]; ok {
					if order < minVocabOrder {
						minVocabOrder = order
						minIdx = i
					}
				}
			}

			if minIdx >= 0 {
				// If we find a pair to merge, we merge it in word, leaving the other
				// tokens intact, and run again...
				tokenText := word[minIdx] + word[minIdx+1]
				word = slices.Replace(word, minIdx, minIdx+2, tokenText)
			} else {
				// No more pairs found to merge; we're done with this word.
				break
			}
		}

		// Now we're guaranteed to have the word consist of tokens which appear in
		// vocab (since we assume vocab contains each single byte as a token too).
		// Convert the word into a list of token IDs.
		for _, tokenText := range word {
			if token, ok := vocab[tokenText]; ok {
				tokens = append(tokens, token)
			} else {
				panic(fmt.Sprintf("token text %q not found in word %q", tokenText, word))
			}
		}
	}

	return tokens
}

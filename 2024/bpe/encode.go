package bpe

import (
	"fmt"
	"slices"

	"github.com/dlclark/regexp2"
)

const debugEncode = false

func encode(text string, vocab map[string]int, splitPattern string) []int {
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

	for _, word := range words {
		for {
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
				tokenText := word[minIdx] + word[minIdx+1]
				word = slices.Replace(word, minIdx, minIdx+2, tokenText)
			} else {
				break
			}
		}

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

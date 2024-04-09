package bpe

import (
	"fmt"

	"github.com/dlclark/regexp2"
)

var debugTrain = false

type stringPair [2]string

// train trains a BPE tokenizer from the given text. vocabSize is the target
// vocabulary size to learn (total number of tokens the encoder will use,
// including 256 initial tokens for single-byte letters. splitPattern is
// the regexp pattern to use for the initial splitting of text to words.
// Returns a vocabulary: mapping tokens to unique integer IDs.
func train(text string, vocabSize int, splitPattern string) map[string]int {
	// Initially populate the vocabulary with all single-byte tokens.
	if vocabSize < 256 {
		panic("vocabSize must be at least 256, to represent all bytes")
	}
	vocab := make(map[string]int)
	for i := range 256 {
		vocab[string(rune(i))] = i
	}

	// Throughout the training process we keep a list of words that represents
	// the input text. Each word is a list of tokens.
	// * It starts by splitting the input into words using the given splitPattern
	// * Then, each word is split further into a list of single bytes
	//
	// The BPE algorithm will iteratively combine the single-byte tokens
	// into longer, learned tokens with time.
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

	if debugTrain {
		fmt.Println("first 20 words")
		for _, word := range words {
			fmt.Printf("%q\n", word)
		}
	}

	// Iterate BPE training until the vocabulary size reaches the desired level,
	// or stop when no more pairs are found in the input (this can happen if the
	// input text is small relatively to the vocabulary size).
	for len(vocab) < vocabSize {
		// Find the most commonly appearing pair of adjacent tokens in words.
		count := make(map[stringPair]int)
		maxCount := -1
		var maxPair stringPair
		for _, word := range words {
			for i := 0; i < len(word)-1; i++ {
				pair := stringPair{word[i], word[i+1]}
				count[pair] += 1
				if count[pair] > maxCount {
					maxCount = count[pair]
					maxPair = pair
				}
			}
		}
		if maxCount < 1 {
			break
		}

		// Here maxPair is the most frequently seen pair for this round; we learn
		// a new token from this pair.
		newToken := maxPair[0] + maxPair[1]
		vocab[newToken] = len(vocab)

		for i, word := range words {
			// For each word in our list, we "fix" it by using the newly learned
			// token where the pair it replaces appears.
			var fixedWord []string

			j := 0
			for j < len(word)-1 {
				if word[j] == maxPair[0] && word[j+1] == maxPair[1] {
					fixedWord = append(fixedWord, newToken)
					j += 2
				} else {
					fixedWord = append(fixedWord, word[j])
					j++
				}
			}
			if j == len(word)-1 {
				fixedWord = append(fixedWord, word[j])
			}

			words[i] = fixedWord
		}

		if debugTrain {
			fmt.Printf("found maxPair = %q,%q\n", maxPair[0], maxPair[1])
			fmt.Println("first 20 words")
			for _, word := range words {
				fmt.Printf("%q\n", word)
			}
		}
	}

	return vocab
}

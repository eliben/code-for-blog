package bpe

import (
	"fmt"

	"github.com/dlclark/regexp2"
)

type stringPair [2]string

func (sp stringPair) concat() string {
	return sp[0] + sp[1]
}

func train(text string, vocabSize int, splitPattern string) []string {
	var words [][]string

	if vocabSize < 256 {
		panic("vocabSize must be at least 256, to represent all bytes")
	}
	vocab := make(map[string]int)
	for i := range 256 {
		vocab[string(rune(i))] = i
	}

	re := regexp2.MustCompile(splitPattern, regexp2.None)
	m, _ := re.FindStringMatch(text)
	for m != nil {
		var word []string
		for _, r := range m.Runes() {
			word = append(word, string(r))
		}
		words = append(words, word)
		m, _ = re.FindNextMatch(m)
	}

	fmt.Println("first 20 words")
	for _, word := range words {
		fmt.Printf("%q\n", word)
	}

	for len(vocab) < vocabSize {
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

		// here maxPair is the most frequently seen pair
		vocab[maxPair.concat()] = len(vocab)

		for i, word := range words {
			var newWord []string

			j := 0
			for j < len(word)-1 {
				if word[j] == maxPair[0] && word[j+1] == maxPair[1] {
					newWord = append(newWord, maxPair.concat())
					j += 2
				} else {
					newWord = append(newWord, word[j])
					j++
				}
			}
			if j == len(word)-1 {
				newWord = append(newWord, word[j])
			}

			words[i] = newWord
		}

		fmt.Printf("found maxPair = %q,%q\n", maxPair[0], maxPair[1])
		fmt.Println("first 20 words")
		for _, word := range words {
			fmt.Printf("%q\n", word)
		}
	}

	return []string{}
}

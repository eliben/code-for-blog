// Go suffixarray sample - basic usage of creation and lookup.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"fmt"
	"index/suffixarray"
	"regexp"
	"strings"
)

// getStringFromIndex finds the string in data based on given index.
// The index points into data somewhere inside a string that has a \x00 to its
// left and to its right.
// Assumes data is well formed and index is in bounds.
func getStringFromIndex(data []byte, index int) string {
	var start, end int
	for i := index - 1; i >= 0; i-- {
		if data[i] == 0 {
			start = i + 1
			break
		}
	}
	for i := index + 1; i < len(data); i++ {
		if data[i] == 0 {
			end = i
			break
		}
	}
	return string(data[start:end])
}

func main() {
	words := []string{
		"banana",
		"apple",
		"pear",
		"tangerine",
		"orange",
		"lemon",
		"peach",
		"persimmon",
	}

	// Combine all words into a single byte slice, separated by \x00 bytes (which
	// do not appear in words), adding one on each end too.
	data := []byte("\x00" + strings.Join(words, "\x00") + "\x00")
	sa := suffixarray.New(data)

	indices := sa.Lookup([]byte("an"), -1)
	if len(indices) > 0 {
		fmt.Println("Lookup returns:", indices)
	} else {
		fmt.Println("Lookup: not found")
	}

	// Reconstruct matches from indices found by Lookup.
	for _, idx := range indices {
		fmt.Println(getStringFromIndex(data, idx))
	}

	// Here using a completely "literal" regexp, similar to the usage of Lookup,
	// to compare what the two methods return. FindAllIndex can take an arbitrary
	// regexp - but beware of the caveat discussed in the blog post.
	r := regexp.MustCompile("an")
	matches := sa.FindAllIndex(r, -1)
	fmt.Println("FindAllIndex returns:", matches)
}

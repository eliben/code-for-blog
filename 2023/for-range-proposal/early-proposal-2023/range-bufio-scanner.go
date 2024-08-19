// Example: iterator over bufio.Scanner
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"os"
)

func main() {
	scanner := newScanner(os.Stdin)
	for line := range scanner.All {
		fmt.Println("got line:", line)
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("reading stdin: %v", err)
	}
}

type myScanner struct {
	s *bufio.Scanner
}

func newScanner(r io.Reader) *myScanner {
	s := bufio.NewScanner(r)
	return &myScanner{
		s: s,
	}
}

func (ms *myScanner) All(yield func(string) bool) bool {
	for ms.s.Scan() {
		if !yield(ms.s.Text()) {
			return false
		}
	}
	return true
}

func (ms *myScanner) Err() error {
	return ms.s.Err()
}

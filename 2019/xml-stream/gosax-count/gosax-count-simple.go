package main

import (
	"fmt"
	"os"
	"strings"

	"github.com/eliben/gosax"
)

func main() {
	counter := 0
	inLocation := false

	scb := gosax.SaxCallbacks{
		StartElement: func(name string, attrs []string) {
			if name == "location" {
				inLocation = true
			} else {
				inLocation = false
			}
		},

		EndElement: func(name string) {
			inLocation = false
		},

		Characters: func(contents string) {
			if inLocation && strings.Contains(contents, "Africa") {
				counter++
			}
		},
	}

	err := gosax.ParseFile(os.Args[1], scb)
	if err != nil {
		panic(err)
	}

	fmt.Println("counter =", counter)
}

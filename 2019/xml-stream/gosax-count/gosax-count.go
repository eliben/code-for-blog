// An example using gosax, with the optimized callbacks.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"fmt"
	"os"
	"strings"
	"unsafe"

	"github.com/eliben/gosax"
)

func main() {
	counter := 0
	inLocation := false

	scb := gosax.SaxCallbacks{
		// We're not interested in attributes here, only tag names; register
		// the StartElementNoAttr callback because it's faster than StartElement.
		StartElementNoAttr: func(name string) {
			if name == "location" {
				inLocation = true
			} else {
				inLocation = false
			}
		},

		EndElement: func(name string) {
			inLocation = false
		},

		// Use CharactersRaw to speed things up, since we're only interested in
		// unpacking the character data inside 'location' tags, not for every
		// tag.
		CharactersRaw: func(ch unsafe.Pointer, chlen int) {
			if inLocation {
				if strings.Contains(gosax.UnpackString(ch, chlen), "Africa") {
					counter++
				}
			}
		},
	}

	err := gosax.ParseFile(os.Args[1], scb)
	if err != nil {
		panic(err)
	}

	fmt.Println("counter =", counter)
}

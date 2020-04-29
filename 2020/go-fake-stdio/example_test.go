// Example of using fakestdio.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package fakestdio

import (
	"fmt"
	"log"
)

func ExampleFakeInOut() {
	// Create a new fakestdio with some input to feed into Stdin.
	fs, err := New("input text")
	if err != nil {
		log.Fatal(err)
	}

	// Scan from Stdin. Beware of scanning too much without closing Stdin, or the
	// program may hang. Consider calling fs.CloseStdin() if needed.
	var scanned string
	fmt.Scanf("%s", &scanned)

	// Emit text to Stdout - it will be captured.
	fmt.Print("some output")

	b, err := fs.ReadAndRestore()
	if err != nil {
		log.Fatal(err)
	}

	// This will go to the actual os.Stdout because we're no longer capturing.
	fmt.Printf("Scanned: %q, Captured: %q", scanned, string(b))

	// Output: Scanned: "input", Captured: "some output"
}

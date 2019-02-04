// Encode into a stream, using io.Writer and json.Encoder.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"encoding/base64"
	"encoding/json"
	"os"
)

func main() {
	// Create an io.Writer that encodes bytes written into it as base64 and emits
	// them to stdout.
	base64writer := base64.NewEncoder(base64.StdEncoding, os.Stdout)

	// Create a new JSON encoder, hooking up its output to base64writer.
	je := json.NewEncoder(base64writer)
	je.Encode(map[string]float64{"foo": 4.12, "pi": 3.14159})

	// Flush any partially encoded blocks left in the base64 encoder.
	base64writer.Close()
}

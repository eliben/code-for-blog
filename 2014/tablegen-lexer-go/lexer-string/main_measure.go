// This main() function runs tokenizeAllPrealloc once, for the sake of
// fine-grained GC tracing with env vars like GODEBUG.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"time"
)

func main() {
	var inputFilename = os.Getenv("TDINPUT")
	buf, err := ioutil.ReadFile(inputFilename)
	if err != nil {
		log.Fatal(err)
	}
	sbuf := string(buf)

	t1 := time.Now()
	tokenizeAllPrealloc(sbuf)
	elapsed := time.Now().Sub(t1)
	fmt.Println(elapsed)
}

package main

import (
	"fmt"
	"log"
	"os"
)

func main() {
	r, w, err := os.Pipe()
	if err != nil {
		log.Fatal(err)
	}
	origStdout := os.Stdout
	os.Stdout = w

	fmt.Print("hello to stdout")

	buf := make([]byte, 1024)
	n, err := r.Read(buf)
	if err != nil {
		log.Fatal(err)
	}

	// Restore
	os.Stdout = origStdout

	fmt.Println("Written to stdout:", string(buf[:n]))
}

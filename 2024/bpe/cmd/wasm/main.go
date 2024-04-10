package main

import (
	_ "embed"
	"fmt"
	"log"
	"strings"
	"sync"
	"syscall/js"

	"example.com/bpe"
)

//go:embed embed_data/cl100k_base.tiktoken
var vocabFileData string

var vocab map[string]int

func main() {
	var once sync.Once
	once.Do(func() {
		v, err := bpe.LoadTiktokenVocab(strings.NewReader(vocabFileData))
		if err != nil {
			log.Fatal(err)
		}
		vocab = v

		fmt.Printf("vocabulary loaded, len=%v\n", len(vocab))
	})

	js.Global().Set("itsAlive", jsItsAlive)

	// For the Go code to be usable from JS, the main function has to run forever.
	<-make(chan bool)
}

var jsItsAlive = js.FuncOf(func(this js.Value, args []js.Value) interface{} {
	result := itsAlive()
	return result
})

func itsAlive() string {
	fmt.Printf("vocabulary loaded, len=%v\n", len(vocab))
	return "go is alive"
}

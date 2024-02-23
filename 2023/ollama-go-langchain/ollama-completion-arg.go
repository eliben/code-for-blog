// Sample of using Ollama's completion API through LangChainGo.
// Takes query from argv.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"context"
	"flag"
	"fmt"
	"log"

	"github.com/tmc/langchaingo/llms"
	"github.com/tmc/langchaingo/llms/ollama"
)

func main() {
	modelName := flag.String("model", "", "ollama model name")
	flag.Parse()

	llm, err := ollama.New(ollama.WithModel(*modelName))
	if err != nil {
		log.Fatal(err)
	}

	query := flag.Args()[0]
	ctx := context.Background()
	completion, err := llms.GenerateFromSinglePrompt(ctx, llm, query)
	if err != nil {
		log.Fatal(err)
	}

	fmt.Println("Response:\n", completion)
}

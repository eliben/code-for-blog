// Sample of using Ollama's completion API through LangChainGo.
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
	modelName := flag.String("model", "llama2", "ollama model name")
	flag.Parse()

	llm, err := ollama.New(ollama.WithModel(*modelName))
	if err != nil {
		log.Fatal(err)
	}

	query := "very briefly, tell me the difference between a comet and a meteor"

	ctx := context.Background()
	completion, err := llms.GenerateFromSinglePrompt(ctx, llm, query)
	if err != nil {
		log.Fatal(err)
	}

	fmt.Println("Response:\n", completion)
}

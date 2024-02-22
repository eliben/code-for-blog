// Sample of using Ollama's streaming completion API through LangChainGo.
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
	_, err = llms.GenerateFromSinglePrompt(ctx, llm, query,
		llms.WithStreamingFunc(func(ctx context.Context, chunk []byte) error {
			fmt.Printf("chunk len=%d: %s\n", len(chunk), chunk)
			return nil
		}))
	if err != nil {
		log.Fatal(err)
	}
}

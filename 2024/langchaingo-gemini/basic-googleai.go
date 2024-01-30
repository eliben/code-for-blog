// Basic example with a single text prompt using the Google AI provider.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"context"
	"fmt"
	"log"
	"os"

	"github.com/tmc/langchaingo/llms"
	"github.com/tmc/langchaingo/llms/googleai"
)

func main() {
	ctx := context.Background()
	apiKey := os.Getenv("API_KEY")
	llm, err := googleai.New(ctx, googleai.WithAPIKey(apiKey))
	if err != nil {
		log.Fatal(err)
	}

	prompt := "What is the L2 Lagrange point?"
	answer, err := llms.GenerateFromSinglePrompt(ctx, llm, prompt)
	if err != nil {
		log.Fatal(err)
	}

	fmt.Println(answer)
}

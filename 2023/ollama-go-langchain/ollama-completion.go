package main

import (
	"context"
	"fmt"
	"log"

	"github.com/tmc/langchaingo/llms/ollama"
)

func main() {
	llm, err := ollama.New(ollama.WithModel("llama2"))
	if err != nil {
		log.Fatal(err)
	}

	query := "very briefly, tell me the difference between a comet and a meteor"

	ctx := context.Background()
	completion, err := llm.Call(ctx, query)
	if err != nil {
		log.Fatal(err)
	}

	fmt.Println("Response:\n", completion)
}

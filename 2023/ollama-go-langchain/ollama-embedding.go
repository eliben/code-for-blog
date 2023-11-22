// Sample of using Ollama's embeddings API through LangChainGo.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
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

	texts := []string{
		"meteor",
		"comet",
		"puppy",
	}

	ctx := context.Background()
	embs, err := llm.CreateEmbedding(ctx, texts)
	if err != nil {
		log.Fatal(err)
	}

	fmt.Printf("Got %d embeddings:\n", len(embs))
	for i, emb := range embs {
		fmt.Printf("%d: len=%d; first few=%v\n", i, len(emb), emb[:4])
	}
}

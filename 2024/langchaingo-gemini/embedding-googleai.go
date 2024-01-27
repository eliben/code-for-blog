package main

import (
	"context"
	"fmt"
	"log"
	"os"

	"github.com/tmc/langchaingo/llms/googleai"
)

func main() {
	ctx := context.Background()
	apiKey := os.Getenv("API_KEY")
	llm, err := googleai.New(ctx, googleai.WithAPIKey(apiKey))
	if err != nil {
		log.Fatal(err)
	}

	texts := []string{"lion", "parrot"}
	emb, err := llm.CreateEmbedding(ctx, texts)
	if err != nil {
		log.Fatal(err)
	}

	fmt.Println("Num of embedding vectors:", len(emb))
	for i, e := range emb {
		fmt.Printf("%d: %v...\n", i, e[:10])
	}
}

// Embeddings.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"context"
	"fmt"
	"log"
	"os"

	"github.com/tmc/langchaingo/llms/googleai/vertex"
)

func main() {
	ctx := context.Background()
	project := os.Getenv("VERTEX_PROJECT")
	location := os.Getenv("VERTEX_LOCATION")
	llm, err := vertex.New(ctx, vertex.WithCloudProject(project), vertex.WithCloudLocation(location))
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

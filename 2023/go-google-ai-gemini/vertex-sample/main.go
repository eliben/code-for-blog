// Example of using the Vertex AI Gemini model for multi-modal content.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"context"
	"encoding/json"
	"fmt"
	"log"
	"os"

	"cloud.google.com/go/vertexai/genai"
)

func main() {
	ctx := context.Background()
	client, err := genai.NewClient(ctx, os.Getenv("GCP_PROJECT_ID"), "us-central1")
	if err != nil {
		log.Fatal(err)
	}
	defer client.Close()

	model := client.GenerativeModel("gemini-pro-vision")

	imgData1, err := os.ReadFile("../images/turtle1.png")
	if err != nil {
		log.Fatal(err)
	}

	imgData2, err := os.ReadFile("../images/turtle2.png")
	if err != nil {
		log.Fatal(err)
	}

	prompt := []genai.Part{
		genai.ImageData("png", imgData1),
		genai.ImageData("png", imgData2),
		genai.Text("Describe the difference between these two pictures, with scientific detail"),
	}
	resp, err := model.GenerateContent(ctx, prompt...)

	if err != nil {
		log.Fatal(err)
	}

	bs, _ := json.MarshalIndent(resp, "", "    ")
	fmt.Println(string(bs))
}

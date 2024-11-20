package main

import (
	"flag"
	"fmt"
	"log"
	"strings"
	"time"

	"github.com/gomlx/gemma/download/kaggle"
	"github.com/gomlx/gemma/samplers"
	"github.com/gomlx/gemma/sentencepiece"
	"github.com/gomlx/gomlx/backends"
	"github.com/gomlx/gomlx/ml/context"

	_ "github.com/gomlx/gomlx/backends/xla"
)

var (
	flagDataDir   = flag.String("data", "", "dir with converted weights")
	flagVocabFile = flag.String("vocab", "", "tokenizer vocabulary file")
)

func main() {
	flag.Parse()
	ctx := context.New()

	// Load model weights from the checkpoint downloaded from Kaggle.
	err := kaggle.ReadConvertedWeights(ctx, *flagDataDir)
	if err != nil {
		log.Fatal(err)
	}

	// Load tokenizer vocabulary.
	vocab, err := sentencepiece.NewFromPath(*flagVocabFile)
	if err != nil {
		log.Fatal(err)
	}

	// Create a Gemma sampler and start sampling tokens.
	sampler, err := samplers.New(backends.New(), ctx, vocab, 256)
	if err != nil {
		log.Fatalf("%+v", err)
	}

	start := time.Now()
	output, err := sampler.Sample([]string{
		"Are bees and wasps similar?",
	})
	if err != nil {
		log.Fatalf("%+v", err)
	}
	fmt.Printf("\tElapsed time: %s\n", time.Since(start))
	fmt.Printf("Generated text:\n%s\n", strings.Join(output, "\n\n"))
}

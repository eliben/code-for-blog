// Loads some random sample images and labels from the CIFAR-10 dataset and
// emits them into .png files in the current dir.
package main

import (
	"flag"
	"fmt"
	"image"
	"image/png"
	"io"
	"log"
	"os"

	"github.com/gomlx/gomlx/backends"
	_ "github.com/gomlx/gomlx/backends/xla"
	"github.com/gomlx/gomlx/examples/cifar"
	"github.com/gomlx/gomlx/types/tensors/images"
	"github.com/gomlx/gopjrt/dtypes"
)

var flagDataDir = flag.String("data", "", "directory to hold downloaded CIFAR data in")
var flagNum = flag.Int("num", 15, "number of images to emit")

func writeImagePNG(img image.Image, filename string) {
	f, err := os.Create(filename)
	if err != nil {
		panic(err)
	}
	defer f.Close()

	if err := png.Encode(f, img); err != nil {
		panic(err)
	}
}

func main() {
	flag.Parse()
	backend := backends.New()
	ds10 := cifar.NewDataset(backend, "Samples Cifar-10", *flagDataDir, cifar.C10, dtypes.Float32, cifar.Test).Shuffle()

	for i := range *flagNum {
		_, inputs, labels, err := ds10.Yield()
		if err == io.EOF {
			fmt.Println("EOF")
			break
		} else if err != nil {
			panic(err)
		}

		if len(labels) != 1 || len(inputs) != 1 {
			log.Fatal("lens not 1")
		}
		inp0, lab0 := inputs[0], labels[0]
		labelStr := cifar.C10Labels[lab0.Value().([]int64)[0]]

		writeImagePNG(images.ToImage().Single(inp0), fmt.Sprintf("img%03d-%s.png", i, labelStr))
		fmt.Printf("img%03d: %s\n", i, labelStr)
	}
}

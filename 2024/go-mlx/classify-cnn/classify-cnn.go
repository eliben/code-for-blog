package main

import (
	"flag"
	"fmt"
	"image"
	"image/png"
	"log"
	"os"
	"path/filepath"
	"strings"

	"example.com/cnnmodel"
	"github.com/gomlx/gomlx/backends"
	_ "github.com/gomlx/gomlx/backends/xla"
	"github.com/gomlx/gomlx/graph"
	mlxcontext "github.com/gomlx/gomlx/ml/context"
	"github.com/gomlx/gomlx/ml/context/checkpoints"
	"github.com/gomlx/gomlx/types/tensors"
	"github.com/gomlx/gomlx/types/tensors/images"
	"github.com/gomlx/gopjrt/dtypes"
)

var C10Labels = []string{"airplane", "automobile", "bird", "cat", "deer", "dog", "frog", "horse", "ship", "truck"}

func main() {
	flagCheckpoint := flag.String("checkpoint", "", "Directory to load checkpoint from")
	flag.Parse()

	mlxctx := mlxcontext.New()
	backend := backends.New()

	_, err := checkpoints.Load(mlxctx).Dir(*flagCheckpoint).Done()
	if err != nil {
		panic(err)
	}
	mlxctx = mlxctx.Reuse() // helps sanity check the loaded context
	exec := mlxcontext.NewExec(backend, mlxctx.In("model"), func(mlxctx *mlxcontext.Context, image *graph.Node) *graph.Node {
		// Convert our image to a tensor with batch dimension of size 1, and pass
		// it to the C10ConvModel graph.
		image = graph.ExpandAxes(image, 0) // Create a batch dimension of size 1.
		logits := cnnmodel.C10ConvModel(mlxctx, nil, []*graph.Node{image})[0]
		// Take the class with highest logit value, then remove the batch dimension.
		choice := graph.ArgMax(logits, -1, dtypes.Int32)
		return graph.Reshape(choice)
	})

	// classify takes a 32x32 image and returns a Cifar-10 classification according
	// to the models. Use C10Labels to convert the returned class to a string
	// name. The returned class is from 0 to 9.
	classify := func(img image.Image) int32 {
		input := images.ToTensor(dtypes.Float32).Single(img)
		outputs := exec.Call(input)
		classID := tensors.ToScalar[int32](outputs[0])
		return classID
	}

	// Classify each .png image found in the given directory
	dirname := flag.Args()[0]
	entries, err := os.ReadDir(dirname)
	if err != nil {
		panic(err)
	}
	for _, path := range entries {
		if !path.IsDir() && strings.HasSuffix(path.Name(), ".png") {
			imgpath := filepath.Join(dirname, path.Name())
			f, err := os.Open(imgpath)
			if err != nil {
				log.Fatal("open error:", err)
			}
			defer f.Close()

			img, err := png.Decode(f)
			if err != nil {
				log.Fatal("decode error:", err)
			}
			n := classify(img)

			fmt.Printf("path=%q, n=%d, label=%q\n", imgpath, n, C10Labels[n])
		}
	}
}

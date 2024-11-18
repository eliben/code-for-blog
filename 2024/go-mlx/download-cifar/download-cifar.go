package main

import (
	"flag"
	"os"

	"github.com/gomlx/gomlx/examples/cifar"
	"github.com/gomlx/gomlx/ml/data"
	"github.com/janpfeifer/must"
)

var flagDataDir = flag.String("data", "", "directory to hold downloaded CIFAR data in")

func main() {
	flag.Parse()
	if !data.FileExists(*flagDataDir) {
		must.M(os.MkdirAll(*flagDataDir, 0777))
	}

	must.M(cifar.DownloadCifar10(*flagDataDir))
}

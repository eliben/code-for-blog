// Download CIFAR-10 data sets into a local directory.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// Much of this code is based on GoMLX samples, and follows its
// Apache License 2.0 [https://github.com/gomlx/gomlx/blob/main/LICENSE]
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

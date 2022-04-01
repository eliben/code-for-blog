// Main runner for profiling and disassembly.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"flag"
	"log"
	"math/rand"
	"os"
	"runtime/pprof"
	"sort"
)

func main() {
	var cpuprofile = flag.String("cpuprofile", "", "write cpu profile to file")
	var n = flag.Int("n", 20000, "slice size")
	var kind = flag.String("kind", "",
		"kind of run: strgeneric, strinterface, strfunc, structgeneric, structinterface")
	flag.Parse()
	rand.Seed(1)

	if *cpuprofile != "" {
		f, err := os.Create(*cpuprofile)
		if err != nil {
			log.Fatal(err)
		}
		pprof.StartCPUProfile(f)
		defer pprof.StopCPUProfile()
	}

	switch *kind {
	case "strgeneric":
		ss := makeRandomStrings(*n)
		bubbleSortGeneric(ss)
		consumeStrings(ss)
	case "strinterface":
		ss := makeRandomStrings(*n)
		bubbleSortInterface(sort.StringSlice(ss))
		consumeStrings(ss)
	case "strfunc":
		ss := makeRandomStrings(*n)
		bubbleSortFunc(ss, func(a, b string) bool { return a < b })
		consumeStrings(ss)
	case "structinterface":
		ss := makeRandomStructs(*n)
		bubbleSortInterface(myStructs(ss))
		consumeStructs(ss)
	case "structgeneric":
		ss := makeRandomStructs(*n)
		bubbleSortFunc(ss, func(a, b *myStruct) bool { return a.n < b.n })
		consumeStructs(ss)
	default:
		flag.Usage()
		log.Fatal("Unsupported kind:", *kind)
	}
}

//go:noinline
func consumeStrings(ss []string) {
}

//go:noinline
func consumeStructs(ss []*myStruct) {
}

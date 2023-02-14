package main

import (
	"context"
	_ "embed"
	"fmt"
	"log"

	"github.com/tetratelabs/wazero"
)

//go:embed table.wasm
var wasmObj []byte

func main() {
	ctx := context.Background()

	// Create a new WebAssembly Runtime.
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)

	_, err := r.NewHostModuleBuilder("env").
		NewFunctionBuilder().
		WithFunc(func(v int32) int32 {
			return v * 3
		}).Export("jstimes3").Instantiate(ctx)

	if err != nil {
		log.Fatal(err)
	}

	tableWasm, err := r.InstantiateModuleFromBinary(ctx, wasmObj)
	if err != nil {
		log.Fatal(err)
	}

	var n uint64 = 12

	r2, err := tableWasm.ExportedFunction("times2").Call(ctx, n)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Printf("times2(%v) = %v\n", n, r2[0])

	r3, err := tableWasm.ExportedFunction("times3").Call(ctx, n)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Printf("times3(%v) = %v\n", n, r3[0])
}

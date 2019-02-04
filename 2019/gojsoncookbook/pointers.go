// Encoding/decoding pointers.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"encoding/json"
	"fmt"
)

type NamePtr struct {
	Id   int
	Name *string
}

// This demonstrates a slice contained in a struct, initially unallocated.
type BoolAndVals struct {
	Fresh bool
	Vals  []float64
}

func main() {
	// The string "Sam" assigned to a variable so that we can take its address
	// while initializing a NamePtr.
	name := "Sam"
	np := NamePtr{101, &name}

	npS, _ := json.Marshal(np)
	fmt.Println(string(npS))

	var npD NamePtr
	if err := json.Unmarshal(npS, &npD); err != nil {
		panic(err)
	}
	fmt.Println(npD.Id, *npD.Name)

	bb := []byte(`
	{
		"Fresh": true,
		"Vals": [1.2, 3.24, 18.99]
	}`)
	var bvD BoolAndVals
	if err := json.Unmarshal(bb, &bvD); err != nil {
		panic(err)
	}
	fmt.Println(bvD)
}

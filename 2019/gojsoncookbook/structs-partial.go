// Encoding/decoding structs without default-valued fields.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"encoding/json"
	"fmt"
)

type Food struct {
	Id             int
	Name           string
	FatPerServ     float64
	ProteinPerServ float64
	CarbPerServ    float64
}

func main() {
	bb := []byte(`
	{
		"Name": "Broccoli",
		"FatPerServ": 0.3,
		"ProteinPerServ": 2.5,
		"CarbPerServ": 3.5
	}`)
	var fD Food
	if err := json.Unmarshal(bb, &fD); err != nil {
		panic(err)
	}
	fmt.Println("unmarshaled Food:", fD)
}

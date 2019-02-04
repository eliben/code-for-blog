// Encoding/decoding slices.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"encoding/json"
	"fmt"
)

func main() {
	sS, _ := json.Marshal([]string{"broccoli", "almonds", "banana"})
	fmt.Println(string(sS))

	var s []string
	if err := json.Unmarshal(sS, &s); err != nil {
		panic(err)
	}
	fmt.Println("unmarshaled []string:", s)

	variedEncodedSlice := []byte(`["broccoli", true]`)

	// This will panic if uncommented!
	//var s2 []string
	//if err := json.Unmarshal(variedEncodedSlice, &s2); err != nil {
	//panic(err)
	//}
	//fmt.Println("unmarshaled []string:", s2)

	var iis []interface{}
	if err := json.Unmarshal(variedEncodedSlice, &iis); err != nil {
		panic(err)
	}
	fmt.Println("unmarshalled slice of length:", len(iis))
	for i, e := range iis {
		fmt.Printf("decoding element %d\n", i)
		switch v := e.(type) {
		case bool:
			fmt.Println("  it's a bool:", v)
		case string:
			fmt.Println("  it's a string:", v)
		// other possible types enumerated...
		default:
			panic("can't figure out the type")
		}
	}
}

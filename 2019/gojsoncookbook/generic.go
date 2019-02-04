// Basic generic encoding/decoding with interface{}.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"encoding/json"
	"fmt"
)

func main() {
	var ii interface{}
	ii = true
	boolS, _ := json.Marshal(ii)
	fmt.Println(string(boolS))

	var ib interface{}
	if err := json.Unmarshal(boolS, &ib); err != nil {
		panic(err)
	}
	b := ib.(bool)
	fmt.Println("unmarshaled bool:", b)

	switch v := ib.(type) {
	case bool:
		fmt.Println("it's a bool:", v)
	case float64:
		fmt.Println("it's a float:", v)
	// other possible types enumerated...
	default:
		panic("can't figure out the type")
	}
}

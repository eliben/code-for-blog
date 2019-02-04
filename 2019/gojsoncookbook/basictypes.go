// Encoding/decoding basic static types.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"encoding/json"
	"fmt"
)

func main() {
	boolS, _ := json.Marshal(true)
	fmt.Println(string(boolS))
	var b bool
	if err := json.Unmarshal(boolS, &b); err != nil {
		panic(err)
	}
	fmt.Println("unmarshaled bool:", b)

	intS, _ := json.Marshal(42)
	fmt.Println(string(intS))
	var i int
	if err := json.Unmarshal(intS, &i); err != nil {
		panic(err)
	}
	fmt.Println("unmarshaled int:", i)

	floatS, _ := json.Marshal(3.14159)
	fmt.Println(string(floatS))
	var f float64
	if err := json.Unmarshal(floatS, &f); err != nil {
		panic(err)
	}
	fmt.Println("unmarshaled float64:", f)

	stringS, _ := json.Marshal("golang")
	fmt.Println(string(stringS))
	var s string
	if err := json.Unmarshal(stringS, &s); err != nil {
		panic(err)
	}
	fmt.Println("unmarshaled string:", s)

	nilS, _ := json.Marshal(nil)
	fmt.Println(string(nilS))
	var p interface{}
	if err := json.Unmarshal(nilS, &p); err != nil {
		panic(err)
	}
	fmt.Println("unmarshaled null:", p)
}

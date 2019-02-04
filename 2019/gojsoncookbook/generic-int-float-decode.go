// Generic decoding demonstrating int/float and UseNumber.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"bytes"
	"encoding/json"
	"fmt"
)

func main() {
	s := []byte("1234")
	var inum interface{}
	if err := json.Unmarshal(s, &inum); err != nil {
		panic(err)
	}
	switch v := inum.(type) {
	case int:
		fmt.Println("it's an int:", v)
	case float64:
		fmt.Println("it's a float:", v)
	// other possible types enumerated...
	default:
		panic("can't figure out the type")
	}

	d := json.NewDecoder(bytes.NewReader(s))
	var ii interface{}
	d.UseNumber()
	if err := d.Decode(&ii); err != nil {
		panic(err)
	}
	switch v := ii.(type) {
	case int:
		fmt.Println("it's an int:", v)
	case float64:
		fmt.Println("it's a float:", v)
	case json.Number:
		fmt.Println("it's a string:", v)
	// other possible types enumerated...
	default:
		panic("can't figure out the type")
	}
}

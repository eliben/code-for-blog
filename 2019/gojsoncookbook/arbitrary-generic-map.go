// Decoding arbitrary nested maps from JSON using generic interface{}s.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"encoding/json"
	"fmt"
)

func findNested(m map[string]interface{}, s string) (bool, interface{}) {
	// Try to find key s at this level
	for k, v := range m {
		if k == s {
			return true, v
		}
	}
	// Not found on this level, so try to find it nested
	for _, v := range m {
		nm, ok := v.(map[string]interface{})
		if ok {
			found, val := findNested(nm, s)
			if found {
				return found, val
			}
		}
	}
	// Not found recursively
	return false, nil
}

func main() {
	// Problem: we don't know at what level "key3" exists.
	b := []byte(`{ "key1": { "key2": { "key3": "Found data" } } } `)
	var f interface{}
	if err := json.Unmarshal(b, &f); err != nil {
		panic(err)
	}
	fmt.Println(f)

	// If f doesn't have this expected type, this will panic
	m := f.(map[string]interface{})
	found, v := findNested(m, "key3")
	fmt.Println(found, v)

	bb := []byte(`
	{
		"foo": true,
		"bar": false,
		"baz": {
			"next": true,
			"prev": {
				"fizz": true,
				"buzz": false
			},
			"top": false
		}
	}`)

	var ii interface{}
	if err := json.Unmarshal(bb, &ii); err != nil {
		panic(err)
	}
	mi := ii.(map[string]interface{})
	ok, fizzVal := findNested(mi, "fizz")
	if ok {
		fmt.Println("found fizz! value is", fizzVal)
	}
}

package main

import (
	"encoding/json"
	"fmt"
	"strings"
)

func main() {
	const s = `
	[
		{"almonds": false},
		{"cashews": true},
		{"walnuts": false}
	]
`
	dec := json.NewDecoder(strings.NewReader(s))

	t, err := dec.Token()
	if err != nil {
		panic(err)
	}
	if t != json.Delim('[') {
		panic("Expected '[' delimiter")
	}

	for dec.More() {
		var m map[string]bool
		err := dec.Decode(&m)
		if err != nil {
			panic(err)
		}

		fmt.Println("decoded", m)
	}

	t, err = dec.Token()
	if err != nil {
		panic(err)
	}
	if t != json.Delim(']') {
		panic("Expected ']' delimiter")
	}
}

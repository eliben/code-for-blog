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

	variedEncodedSlice := []byte(`["broccoli", 25]`)
	var s2 []string
	if err := json.Unmarshal(variedEncodedSlice, &s2); err != nil {
		panic(err)
	}
	fmt.Println("unmarshaled []string:", s2)
}

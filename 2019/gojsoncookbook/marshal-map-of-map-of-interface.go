// Marshaling a deeply nested map.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"encoding/json"
	"fmt"
)

func main() {
	d1 := map[string][]interface{}{
		"a": []interface{}{20, "hello"},
		"b": []interface{}{100}}
	d2 := map[string][]interface{}{
		"x": []interface{}{"str", 10, 20},
	}

	m := make(map[string]map[string][]interface{})
	m["d1"] = d1
	m["d2"] = d2

	s := fmt.Sprintf("%v", m)
	fmt.Println(s)

	b, _ := json.Marshal(m)
	fmt.Println(string(b))
}

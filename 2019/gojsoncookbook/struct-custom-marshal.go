// Custom encoding/marshaling of a struct/type.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"encoding/json"
	"fmt"
)

type Account struct {
	Id   int32
	Name string
}

func (a Account) MarshalJSON() ([]byte, error) {
	m := map[string]string{
		"id":   fmt.Sprintf("0x%08x", a.Id),
		"name": a.Name,
	}

	return json.Marshal(m)
}

func main() {
	joe := Account{Id: 123, Name: "Joe"}
	s, _ := json.Marshal(joe)
	fmt.Println(string(s))
}

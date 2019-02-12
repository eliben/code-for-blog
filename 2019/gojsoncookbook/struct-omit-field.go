// Ommitting a struct field when encoding it.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"encoding/json"
	"fmt"
)

type Account struct {
	Name     string
	Password string `json:"-"`
	Balance  float64
}

func main() {
	joe := Account{Name: "Joe", Password: "123456", Balance: 102.4}
	s, _ := json.Marshal(joe)
	fmt.Println(string(s))
}

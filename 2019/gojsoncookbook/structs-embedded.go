// Unmarhalling with an embedded struct.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"encoding/json"
	"fmt"
)

type Balance struct {
	Dollars int
}

type Account struct {
	Name string
	Balance
}

func main() {
	joe := Account{Name: "Joe", Balance: Balance{Dollars: 100}}
	s, _ := json.Marshal(joe)
	fmt.Println(string(s))

	var a Account
	if err := json.Unmarshal(s, &a); err != nil {
		panic(err)
	}
	fmt.Println(a)
}

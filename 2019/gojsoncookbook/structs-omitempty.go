// Encoding/decoding structs, omitting empty fields.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"encoding/json"
	"fmt"
)

type Food struct {
	Id             int     `json:"id,omitempty"`
	Name           string  `json:"name"`
	FatPerServ     float64 `json:"fat_per_serv"`
	ProteinPerServ float64 `json:"protein_per_serv"`
	CarbPerServ    float64 `json:"carb_per_serv"`
}

func main() {
	f := Food{0, "Broccoli", 0.3, 2.5, 3.5}
	fS, _ := json.MarshalIndent(f, "", "  ")
	fmt.Println(string(fS))
}

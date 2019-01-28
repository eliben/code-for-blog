package main

import (
	"encoding/json"
	"fmt"
)

type Food struct {
	Id             int
	Name           string
	FatPerGram     float64
	ProteinPerGram float64
	CarbPerGram    float64
}

// TODO: show version with nested fields...

func main() {
	f := Food{200403, "Broccoli", 0.3, 2.5, 3.5}
	fS, _ := json.Marshal(f)
	fmt.Println(string(fS))

	var fD Food
	if err := json.Unmarshal(fS, &fD); err != nil {
		panic(err)
	}
	fmt.Println("unmarshaled Food:", fD)
}

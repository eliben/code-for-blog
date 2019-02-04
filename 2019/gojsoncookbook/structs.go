// Encoding/decoding structs.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"encoding/json"
	"fmt"
)

type Food struct {
	Id             int
	Name           string
	FatPerServ     float64
	ProteinPerServ float64
	CarbPerServ    float64
}

type NutrientCount struct {
	FatPerServ     float64
	ProteinPerServ float64
	CarbPerServ    float64
}

type FoodNested struct {
	Id     int
	Name   string
	Ncount NutrientCount
}

func main() {
	f := Food{200403, "Broccoli", 0.3, 2.5, 3.5}
	fS, _ := json.MarshalIndent(f, "", "  ")
	fmt.Println(string(fS))

	var fD Food
	if err := json.Unmarshal(fS, &fD); err != nil {
		panic(err)
	}
	fmt.Println("unmarshaled Food:", fD)

	fn := FoodNested{200489, "Banana", NutrientCount{0.4, 1.3, 23.9}}
	fnS, _ := json.MarshalIndent(fn, "", "  ")
	fmt.Println(string(fnS))

	var fnD FoodNested
	if err := json.Unmarshal(fnS, &fnD); err != nil {
		panic(err)
	}
	fmt.Println("unmarshaled FoodNested:", fnD)
}

// Fieldtags in structs to map to JSON object key names.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"encoding/json"
	"fmt"
)

type Food struct {
	Id             int     `json:"id"`
	Name           string  `json:"name"`
	FatPerServ     float64 `json:"fat_per_serv"`
	ProteinPerServ float64 `json:"protein_per_serv"`
	CarbPerServ    float64 `json:"carb_per_serv"`
}

type NutrientCount struct {
	FatPerServ     float64 `json:"fat_per_serv"`
	ProteinPerServ float64 `json:"protein_per_serv"`
	CarbPerServ    float64 `json:"carb_per_serv"`
}

type FoodNested struct {
	Id     int           `json:"id"`
	Name   string        `json:"name"`
	Ncount NutrientCount `json:"ncount"`
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

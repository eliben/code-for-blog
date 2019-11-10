// Combines delayed with generic parsing to parse the proper struct type.
//
// Shows how to parse structs that can be distinguished by a field value to
// dispatch to the proper type.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"encoding/json"
	"fmt"
)

type FoodItem struct {
	Kind       string
	Name       string
	FatPerServ float64
}

type ArtItem struct {
	Kind   string
	Length int
	Height int
}

type FitnessItem struct {
	Kind        string
	Resistance  float64
	Calibration []int
	Log         []string
}

var kindToPrototype map[string]interface{} = map[string]interface{}{
	"food":    &FoodItem{},
	"art":     &ArtItem{},
	"fitness": &FitnessItem{},
}

// parseToProperStruct unmarshals bb to the proper struct, per the mapping in
// kindToPrototype. Returns an instance of the parsed struct as an interface{}
// (because in the general case these structs don't share an interface). The
// runtime type will vary based on the "Kind" field.
func parseToProperStruct(bb []byte) (interface{}, error) {
	var m map[string]json.RawMessage
	if err := json.Unmarshal(bb, &m); err != nil {
		return nil, err
	}

	if kind, ok := m["Kind"]; ok {
		var skind string
		if err := json.Unmarshal(kind, &skind); err != nil {
			return nil, err
		}

		if prototype, ok := kindToPrototype[skind]; ok {
			if err := json.Unmarshal(bb, prototype); err != nil {
				return nil, err
			}
			return prototype, nil
		} else {
			return nil, fmt.Errorf("unknown kind %v", skind)
		}
	} else {
		return nil, fmt.Errorf("'Kind' field not found")
	}
}

func main() {
	bufs := []string{
		`{
		"Kind": "food",
		"Name": "Broccoli",
		"FatPerServ": 0.3
		}`,
		`{
		"Kind": "fitness",
		"Resistance": 42.5,
		"Calibartion": [7, 8, 1, 0, 30],
		"Log": ["good job", "try harder"]
		}`,
		`{
		"Kind": "art",
		"Length": 100,
		"Height": 22
		}`}

	for _, s := range bufs {
		s, err := parseToProperStruct([]byte(s))
		if err != nil {
			panic(err)
		}
		fmt.Printf("%T ==> %v\n", s, s)
	}
}

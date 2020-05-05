package main

import (
	"encoding/json"
	"fmt"
	"log"
)

var jsonText = []byte(`
{
  "attrs": [
		{
			"name": "color",
			"count": 9
		},
		{
			"name": "family",
			"count": 127
		}],
	"fruits": [
		{
			"name": "orange",
			"sweetness": 12.3,
			"attr": {"family": "citrus"}
		},
		{
			"name": "banana",
			"sweetness": 21.8,
			"attr": {"color": "yellow"}
		}
	]
}`)

func asMapGeneric() {
	fmt.Println("asMapGeneric")
	var m map[string]interface{}
	if err := json.Unmarshal(jsonText, &m); err != nil {
		log.Fatal(err)
	}
	fmt.Println(m)
}

func main() {
	asMapGeneric()
}

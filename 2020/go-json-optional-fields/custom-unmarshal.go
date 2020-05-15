package main

import (
	"encoding/json"
	"fmt"
	"log"
)

var jsonText = []byte(`
{
	"id": "foobar",
	"verbose": false,
	"bug": "oops",
	"level": 10
}`)

type Options struct {
	Id      string `json:"id,omitempty"`
	Verbose bool   `json:"verbose,omitempty"`
	Level   int    `json:"level,omitempty"`
	Power   int    `json:"power,omitempty"`
}

func (o *Options) UnmarshalJSON(text []byte) error {
	var m map[string]interface{}
	if err := json.Unmarshal(text, &m); err != nil {
		return err
	}
	fmt.Println("Unmarshal:", string(text))
	fmt.Println("Map:", m)
	return nil
}

func main() {
	var opts Options
	if err := json.Unmarshal(jsonText, &opts); err != nil {
		log.Fatal(err)
	}
	fmt.Printf("%+v\n", opts)
}

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
	"level": 10
}`)

type Options struct {
	Id      string `json:"id,omitempty"`
	Verbose bool   `json:"verbose,omitempty"`
	Level   int    `json:"level,omitempty"`
	Power   int    `json:"power,omitempty"`
}

func parseOptions(jsn []byte) Options {
	opts := Options{
		Verbose: false,
		Level:   0,
		Power:   10,
	}
	if err := json.Unmarshal(jsn, &opts); err != nil {
		log.Fatal(err)
	}
	return opts
}

func main() {
	opts := parseOptions(jsonText)
	fmt.Println(opts)
}

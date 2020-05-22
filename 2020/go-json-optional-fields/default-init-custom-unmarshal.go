// Set default values prior to parsing with a custom UnmarshalJSON.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
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

func (o *Options) UnmarshalJSON(text []byte) error {
	type options Options
	opts := options{
		Power: 10,
	}
	if err := json.Unmarshal(text, &opts); err != nil {
		return err
	}
	*o = Options(opts)
	return nil
}

func main() {
	var opts Options
	if err := json.Unmarshal(jsonText, &opts); err != nil {
		log.Fatal(err)
	}
	fmt.Println(opts)
}

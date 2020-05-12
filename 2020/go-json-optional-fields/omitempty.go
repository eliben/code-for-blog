package main

import (
	"bytes"
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

func main() {
	// This Unmarshal will succeed even though our Options struct has no "bug"
	// field.
	var opts Options
	if err := json.Unmarshal(jsonText, &opts); err != nil {
		log.Fatal(err)
	}
	fmt.Println(opts)

	// Using an explicit decoder and enabling DisallowUnknownFields will catch
	// the unrecognized field.
	dec := json.NewDecoder(bytes.NewReader(jsonText))
	dec.DisallowUnknownFields()
	var opts2 Options
	if err := dec.Decode(&opts2); err != nil {
		fmt.Println("Decode error:", err)
	}

	jsonOut, _ := json.MarshalIndent(opts, "", "  ")
	fmt.Println(string(jsonOut))
}

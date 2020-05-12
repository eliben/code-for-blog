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
	Id      *string `json:"id,omitempty"`
	Verbose *bool   `json:"verbose,omitempty"`
	Level   *int    `json:"level,omitempty"`
	Power   *int    `json:"power,omitempty"`
}

func main() {
	var opts Options
	if err := json.Unmarshal(jsonText, &opts); err != nil {
		log.Fatal(err)
	}

	if opts.Level != nil {
		fmt.Printf("Level specified: %d\n", *opts.Level)
	}

	if opts.Power != nil {
		fmt.Printf("Power specified: %d\n", *opts.Power)
	}

	opts2 := Options{
		Id:    String("joe"),
		Power: Int(100),
	}

	jsonOut, _ := json.MarshalIndent(opts2, "", "  ")
	fmt.Println(string(jsonOut))
}

func Bool(v bool) *bool       { return &v }
func Int(v int) *int          { return &v }
func String(v string) *string { return &v }

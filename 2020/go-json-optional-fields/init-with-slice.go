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
	"level": 10,

	"regions": [{"name": "Vermont", "power": 12}, {"name": "Abruzzo"}]
}`)

type Region struct {
	Name  string `json:"name,omitempty"`
	Power int    `json:"power,omitempty"`
}

type Options struct {
	Id      string `json:"id,omitempty"`
	Verbose bool   `json:"verbose,omitempty"`
	Level   int    `json:"level,omitempty"`
	Power   int    `json:"power,omitempty"`

	Regions []Region `json:"regions,omitempty"`
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

func (r *Region) UnmarshalJSON(text []byte) error {
	type region Region
	reg := region{
		Power: 10,
	}
	if err := json.Unmarshal(text, &reg); err != nil {
		return err
	}
	*r = Region(reg)
	return nil
}

func main() {
	var opts Options
	if err := json.Unmarshal(jsonText, &opts); err != nil {
		log.Fatal(err)
	}
	fmt.Println(opts)

}

// Custom unmarshaling of a JSON type to set defaults to missing keys.
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
	// Ask json to parse text to a generic map[string]interface{}, which accepts
	// any key and any (JSON-valid) value type.
	var m map[string]interface{}
	if err := json.Unmarshal(text, &m); err != nil {
		return err
	}

	if iid, ok := m["id"]; ok {
		id, ok := iid.(string)
		if !ok {
			return fmt.Errorf("got type %T for 'id', want string", iid)
		}
		o.Id = id
	} else {
		o.Id = ""
	}

	if iverbose, ok := m["verbose"]; ok {
		verbose, ok := iverbose.(bool)
		if !ok {
			return fmt.Errorf("got type %T for 'verbose', want bool", iverbose)
		}
		o.Verbose = verbose
	} else {
		o.Verbose = true
	}

	if ilevel, ok := m["level"]; ok {
		flevel, ok := ilevel.(float64)
		if !ok {
			return fmt.Errorf("got type %T for 'level', want float64", ilevel)
		}
		o.Level = int(flevel)
	} else {
		o.Level = 1
	}

	if ipower, ok := m["power"]; ok {
		fpower, ok := ipower.(float64)
		if !ok {
			return fmt.Errorf("got type %T for 'power', want float64", ipower)
		}
		o.Power = int(fpower)
	} else {
		o.Power = 1
	}

	// TODO: verify no unknown fields

	return nil
}

func main() {
	var opts Options
	if err := json.Unmarshal(jsonText, &opts); err != nil {
		log.Fatal(err)
	}
	fmt.Printf("%+v\n", opts)
}

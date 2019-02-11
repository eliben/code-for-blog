// Delayed parsing/unmarshaling with json.RawMessage
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"encoding/json"
	"fmt"
)

type Event struct {
	Name string `json:"name"`
	Url  string `json:"url"`
}

func main() {
	bb := []byte(`
	{
		"event": {"name": "joe", "url": "event://101"},
		"otherstuff": 15.2,
		"anotherstuff": 100
	}`)

	var m map[string]json.RawMessage
	if err := json.Unmarshal(bb, &m); err != nil {
		panic(err)
	}

	if eventRaw, ok := m["event"]; ok {
		var event Event
		if err := json.Unmarshal(eventRaw, &event); err != nil {
			panic(err)
		}
		fmt.Println("Parsed Event:", event)
	} else {
		fmt.Println("Can't find 'event' key in JSON")
	}
}

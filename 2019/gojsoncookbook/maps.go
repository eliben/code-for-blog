package main

import (
	"encoding/json"
	"fmt"
)

func main() {
	mS, _ := json.Marshal(map[string]bool{"almonds": false, "cashews": true})
	fmt.Println(string(mS))

	var m map[string]bool
	if err := json.Unmarshal(mS, &m); err != nil {
		panic(err)
	}
	fmt.Println("unmarshaled map[string]bool:", m)
}

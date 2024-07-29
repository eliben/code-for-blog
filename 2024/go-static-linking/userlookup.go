package main

import (
	"encoding/json"
	"log"
	"os"
	"os/user"
)

func main() {
	user, err := user.Lookup("eliben")
	if err != nil {
		log.Fatal(err)
	}

	je := json.NewEncoder(os.Stdout)
	je.Encode(user)
}

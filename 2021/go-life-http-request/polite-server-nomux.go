// Polite server without routing.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"fmt"
	"log"
	"net/http"
)

type PoliteServer struct {
}

func (ms *PoliteServer) ServeHTTP(w http.ResponseWriter, req *http.Request) {
	fmt.Fprintf(w, "Welcome! Thanks for visiting!\n")
}

func main() {
	ps := &PoliteServer{}
	log.Fatal(http.ListenAndServe(":8090", ps))
}

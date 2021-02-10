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

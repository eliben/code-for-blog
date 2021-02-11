package main

import (
	"fmt"
	"log"
	"net/http"
)

func politeGreeting(w http.ResponseWriter, req *http.Request) {
	fmt.Fprintf(w, "Welcome! Thanks for visiting!\n")
}

func main() {
	log.Fatal(http.ListenAndServe(":8090", http.HandlerFunc(politeGreeting)))
}

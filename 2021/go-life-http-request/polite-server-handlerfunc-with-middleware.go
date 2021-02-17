// Polite server with basic logging middleware using HandlerFunc.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"fmt"
	"log"
	"net/http"
	"time"
)

func politeGreeting(w http.ResponseWriter, req *http.Request) {
	fmt.Fprintf(w, "Welcome! Thanks for visiting!\n")
}

func loggingMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, req *http.Request) {
		start := time.Now()
		next.ServeHTTP(w, req)
		log.Printf("%s %s %s", req.Method, req.RequestURI, time.Since(start))
	})
}

func main() {
	lm := loggingMiddleware(http.HandlerFunc(politeGreeting))
	log.Fatal(http.ListenAndServe(":8090", lm))
}

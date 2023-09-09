// Basic API server - unaware of CORS.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"fmt"
	"net/http"
)

func apiHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/json")
	fmt.Fprintln(w, `{"message": "hello"}`)
}

func main() {
	port := ":8080"
	mux := http.NewServeMux()
	mux.HandleFunc("/api", apiHandler)
	http.ListenAndServe(port, mux)
}

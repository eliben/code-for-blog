// Sample of using the (new in 1.22) http.ServeMux
// See README.md for invocation instructions.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"fmt"
	"net/http"
)

func main() {
	mux := http.NewServeMux()
	mux.HandleFunc("/task/{id}/status/", func(w http.ResponseWriter, r *http.Request) {
		id := r.PathValue("id")
		fmt.Fprintf(w, "handling task status with id=%v\n", id)
	})
	mux.HandleFunc("/task/0/{action}/", func(w http.ResponseWriter, r *http.Request) {
		action := r.PathValue("action")
		fmt.Fprintf(w, "handling task 0 with action=%v\n", action)
	})

	http.ListenAndServe("localhost:8090", mux)
}

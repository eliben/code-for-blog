// Static file server on the /static/ route, and dynamic serving on another
// route.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"fmt"
	"net/http"
	"time"
)

func timeHandler(w http.ResponseWriter, r *http.Request) {
	fmt.Fprint(w, time.Now().Format("02 Jan 2006 15:04:05 MST")+"\n")
}

func main() {
	port := ":9999"

	fileHandler := http.StripPrefix("/static/", http.FileServer(http.Dir("files")))
	http.Handle("/static/", fileHandler)
	http.HandleFunc("/time", timeHandler)

	http.ListenAndServe(port, nil)
}

// Serve a full web application with a directory of "static" files (HTML, CSS,
// JS) as well as dynamic routes the client-side code communicates with. This
// sample uses Go's embedding to embed the static files into the Go binary.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"embed"
	"fmt"
	"io/fs"
	"log"
	"net/http"
	"time"
)

//go:embed public
var public embed.FS

func timeHandler(w http.ResponseWriter, r *http.Request) {
	fmt.Fprint(w, time.Now().Format("02 Jan 2006 15:04:05 MST"))
}

func main() {
	// We want to serve static content from the root of the 'public' directory,
	// but go:embed will create a FS where all the paths start with 'public/...'.
	// Using fs.Sub we "cd" into 'public' and can serve files relative to it.
	publicFS, err := fs.Sub(public, "public")
	if err != nil {
		log.Fatal(err)
	}

	http.HandleFunc("/time", timeHandler)
	http.Handle("/", http.FileServer(http.FS(publicFS)))

	port := ":9999"
	log.Fatal(http.ListenAndServe(port, nil))
}

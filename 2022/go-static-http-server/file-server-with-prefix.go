// Local file server that serves the files to a non-root path. For this to
// work properly, we have to use the http.StripPrefix middleware which
// rewrites requests to remove the prefix.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import "net/http"

func main() {
	port := ":9999"
	handler := http.StripPrefix("/static/", http.FileServer(http.Dir("files")))
	http.Handle("/static/", handler)
	http.ListenAndServe(port, nil)
}

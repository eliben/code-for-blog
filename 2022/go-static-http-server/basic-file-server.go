// Basic local file server, serving the "static" directory on the root path.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import "net/http"

func main() {
	port := ":9999"
	handler := http.FileServer(http.Dir("files"))
	http.ListenAndServe(port, handler)
}

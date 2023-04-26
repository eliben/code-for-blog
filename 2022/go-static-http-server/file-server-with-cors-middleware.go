// Like basic-file-server, but adds some CORS headers using middleware.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import "net/http"

func addCORS(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, req *http.Request) {
		w.Header().Set("Cross-Origin-Embedder-Policy", "require-corp")
		w.Header().Set("Cross-Origin-Opener-Policy", "same-origin")
		next.ServeHTTP(w, req)
	})
}

func main() {
	port := ":9999"
	handler := addCORS(http.FileServer(http.Dir("files")))
	http.ListenAndServe(port, handler)
}

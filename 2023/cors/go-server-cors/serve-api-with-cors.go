package main

import (
	"fmt"
	"net/http"
)

var originAllowlist = map[string]bool{
	"http://127.0.0.1:9999":    true,
	"http://cats.com":          true,
	"http://safe.frontend.net": true,
}

func checkCORS(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		origin := r.Header.Get("Origin")
		if _, found := originAllowlist[origin]; found {
			w.Header().Set("Access-Control-Allow-Origin", origin)
			w.Header().Add("Vary", "Origin")
		}

		next.ServeHTTP(w, r)
	})
}

func apiHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/json")
	fmt.Fprintln(w, `{"message": "hello"}`)
}

func main() {
	port := ":8080"
	mux := http.NewServeMux()
	mux.HandleFunc("/api", apiHandler)
	http.ListenAndServe(port, checkCORS(mux))
}

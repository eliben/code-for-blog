package main

import (
	"fmt"
	"net/http"
	"slices"
	"strings"
)

var originAllowlist = []string{
	"http://127.0.0.1:9999",
	"http://cats.com",
	"http://safe.frontend.net",
}

var methodAllowlist = []string{"GET", "POST", "DELETE", "OPTIONS"}

func checkCORS(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if isPreflight(r) {
			origin := r.Header.Get("Origin")
			method := r.Header.Get("Access-Control-Request-Method")
			if slices.Contains(originAllowlist, origin) && slices.Contains(methodAllowlist, method) {
				w.Header().Set("Access-Control-Allow-Origin", origin)
				w.Header().Set("Access-Control-Allow-Methods", strings.Join(methodAllowlist, ", "))
				w.Header().Add("Vary", "Origin")
			}
		} else {
			// Not a preflight: regular request.
			origin := r.Header.Get("Origin")
			if slices.Contains(originAllowlist, origin) {
				w.Header().Set("Access-Control-Allow-Origin", origin)
				w.Header().Add("Vary", "Origin")
			}
		}
		next.ServeHTTP(w, r)
	})
}

// isPreflight reports whether r is a preflight requst.
func isPreflight(r *http.Request) bool {
	return r.Method == "OPTIONS" &&
		r.Header.Get("Origin") != "" &&
		r.Header.Get("Access-Control-Request-Method") != ""
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

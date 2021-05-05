package main

import (
	"crypto/tls"
	"flag"
	"fmt"
	"log"
	"net/http"
)

func verifyUserPass(username, password string) bool {
	if username == "joe" && password == "1234" {
		return true
	}
	return false
}

func main() {
	addr := flag.String("addr", ":4000", "HTTPS network address")
	certFile := flag.String("certfile", "cert.pem", "certificate PEM file")
	keyFile := flag.String("keyfile", "key.pem", "key PEM file")
	flag.Parse()

	mux := http.NewServeMux()
	mux.HandleFunc("/", func(w http.ResponseWriter, req *http.Request) {
		if req.URL.Path != "/" {
			http.NotFound(w, req)
			return
		}
		fmt.Fprintf(w, "Proudly served with Go and HTTPS!\n")
	})

	mux.HandleFunc("/secret/", func(w http.ResponseWriter, req *http.Request) {
		user, pass, ok := req.BasicAuth()
		if ok && verifyUserPass(user, pass) {
			fmt.Fprintf(w, "You get to see the secret\n")
		} else {
			w.Header().Set("WWW-Authenticate", `Basic realm="api"`)
			http.Error(w, "Unauthorized", http.StatusUnauthorized)
		}
	})

	srv := &http.Server{
		Addr:    *addr,
		Handler: mux,
		TLSConfig: &tls.Config{
			MinVersion:               tls.VersionTLS13,
			PreferServerCipherSuites: true,
		},
	}

	log.Printf("Starting server on %s", *addr)
	err := srv.ListenAndServeTLS(*certFile, *keyFile)
	log.Fatal(err)
}

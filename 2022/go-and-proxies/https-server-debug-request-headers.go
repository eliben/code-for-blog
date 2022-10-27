package main

import (
	"crypto/tls"
	"flag"
	"fmt"
	"log"
	"net/http"
	"strings"
)

func main() {
	addr := flag.String("addr", "127.0.0.1:8080", "listen address")
	certFile := flag.String("certfile", "cert.pem", "certificate PEM file")
	keyFile := flag.String("keyfile", "key.pem", "key PEM file")
	flag.Parse()

	mux := http.NewServeMux()
	mux.HandleFunc("/",
		func(w http.ResponseWriter, req *http.Request) {
			var b strings.Builder

			fmt.Fprintf(&b, "%v\t%v\t%v\tHost: %v\n", req.RemoteAddr, req.Method, req.URL, req.Host)
			for name, headers := range req.Header {
				for _, h := range headers {
					fmt.Fprintf(&b, "%v: %v\n", name, h)
				}
			}
			log.Println(b.String())

			fmt.Fprintf(w, "HTTPS hello %s\n", req.URL)
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
	if err := srv.ListenAndServeTLS(*certFile, *keyFile); err != nil {
		log.Fatal(err)
	}
}

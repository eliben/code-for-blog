// A TLS-terminating single-backend reverse proxy. Listens on the address given
// with the --from flag and forwards all traffic to the server given with the
// --to flag.
// Similar to basic-reverse-proxy, but talks HTTPS.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"crypto/tls"
	"flag"
	"log"
	"net/http"
	"net/http/httputil"
	"net/url"
	"strings"
)

func main() {
	fromAddr := flag.String("from", "127.0.0.1:9090", "proxy's listening address")
	toAddr := flag.String("to", "127.0.0.1:8080", "the address this proxy will forward to")
	certFile := flag.String("certfile", "cert.pem", "certificate PEM file")
	keyFile := flag.String("keyfile", "key.pem", "key PEM file")
	flag.Parse()

	toUrl := parseToUrl(*toAddr)
	proxy := httputil.NewSingleHostReverseProxy(toUrl)

	srv := &http.Server{
		Addr:    *fromAddr,
		Handler: proxy,
		TLSConfig: &tls.Config{
			MinVersion:               tls.VersionTLS13,
			PreferServerCipherSuites: true,
		},
	}
	log.Println("Starting proxy server on", *fromAddr)
	if err := srv.ListenAndServeTLS(*certFile, *keyFile); err != nil {
		log.Fatal("ListenAndServe:", err)
	}
}

// parseToUrl parses a "to" address to url.URL value
func parseToUrl(addr string) *url.URL {
	if !strings.HasPrefix(addr, "http") {
		addr = "http://" + addr
	}
	toUrl, err := url.Parse(addr)
	if err != nil {
		log.Fatal(err)
	}
	return toUrl
}

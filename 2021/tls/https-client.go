// Simple HTTPS client in Go.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"crypto/tls"
	"crypto/x509"
	"flag"
	"fmt"
	"io"
	"log"
	"net/http"
	"os"
)

func main() {
	addr := flag.String("addr", "localhost:4000", "HTTPS server address")
	certFile := flag.String("certfile", "cert.pem", "trusted CA certificate")
	flag.Parse()

	cert, err := os.ReadFile(*certFile)
	if err != nil {
		log.Fatal(err)
	}
	certPool := x509.NewCertPool()
	if ok := certPool.AppendCertsFromPEM(cert); !ok {
		log.Fatalf("unable to parse cert from %s", *certFile)
	}

	client := &http.Client{
		Transport: &http.Transport{
			TLSClientConfig: &tls.Config{
				RootCAs: certPool,
			},
		},
	}

	r, err := client.Get("https://" + *addr)
	if err != nil {
		log.Fatal(err)
	}
	defer r.Body.Close()

	html, err := io.ReadAll(r.Body)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Printf("%v\n", r.Status)
	fmt.Printf(string(html))
}

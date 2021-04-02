// Simple HTTPS client in Go with mTLS.
//
// The client authenticates itself with its own certificate, as well as checks
// the server's certificate.
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
	certFile := flag.String("certfile", "cert.pem", "server certificate")
	clientCertFile := flag.String("clientcert", "clientcert.pem", "certificate PEM for client")
	clientKeyFile := flag.String("clientkey", "clientkey.pem", "key PEM for client")
	flag.Parse()

	// Load our client certificate and key.
	clientCert, err := tls.LoadX509KeyPair(*clientCertFile, *clientKeyFile)
	if err != nil {
		log.Fatal(err)
	}

	// Trusted server certificate.
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
				RootCAs:      certPool,
				Certificates: []tls.Certificate{clientCert},
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

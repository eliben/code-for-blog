package main

import (
	"crypto/tls"
	"flag"
	"fmt"
	"log"
)

func main() {
	addr := flag.String("addr", "localhost:4040", "dial address")
	flag.Parse()

	cfg := tls.Config{}
	conn, err := tls.Dial("tcp", *addr, &cfg)
	if err != nil {
		log.Fatal("TLS connection failed: " + err.Error())
	}

	certChain := conn.ConnectionState().PeerCertificates
	for i, cert := range certChain {
		fmt.Println(i)
		fmt.Println("Issuer:", cert.Issuer)
		fmt.Println("Subject:", cert.Subject)
		fmt.Println("Version:", cert.Version)
		fmt.Println("NotAfter:", cert.NotAfter)
		fmt.Println("DNS names:", cert.DNSNames)
		fmt.Println("")
	}
}

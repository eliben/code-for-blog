package main

import (
	"crypto/tls"
	"crypto/x509"
	"flag"
	"fmt"
	"io"
	"log"
	"os"
)

func main() {
	port := flag.String("port", "4040", "port to connect")
	certFile := flag.String("certfile", "cert.pem", "server certificate")
	flag.Parse()

	cert, err := os.ReadFile(*certFile)
	if err != nil {
		log.Fatal(err)
	}
	certPool := x509.NewCertPool()
	if ok := certPool.AppendCertsFromPEM(cert); !ok {
		log.Fatalf("unable to parse cert from %s", *certFile)
	}
	config := &tls.Config{RootCAs: certPool}

	conn, err := tls.Dial("tcp", "localhost:"+*port, config)
	if err != nil {
		log.Fatal(err)
	}

	_, err = io.WriteString(conn, "Hello simple secure Server\n")
	if err != nil {
		log.Fatal("client write error:", err)
	}
	if err = conn.CloseWrite(); err != nil {
		log.Fatal(err)
	}

	buf := make([]byte, 256)
	n, err := conn.Read(buf)
	if err != nil && err != io.EOF {
		log.Fatal(err)
	}

	fmt.Println("client read:", string(buf[:n]))
	conn.Close()
}

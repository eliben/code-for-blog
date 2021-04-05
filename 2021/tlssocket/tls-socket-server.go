package main

import (
	"crypto/tls"
	"flag"
	"io"
	"log"
	"net"
)

func main() {
	port := flag.String("port", "4040", "listening port")
	certFile := flag.String("cert", "cert.pem", "certificate PEM file")
	keyFile := flag.String("key", "key.pem", "key PEM file")
	flag.Parse()

	cert, err := tls.LoadX509KeyPair(*certFile, *keyFile)
	if err != nil {
		log.Fatal(err)
	}
	config := &tls.Config{Certificates: []tls.Certificate{cert}}

	log.Printf("listening on port %s\n", *port)
	l, err := tls.Listen("tcp", ":"+*port, config)
	if err != nil {
		log.Fatal(err)
	}
	defer l.Close()

	for {
		conn, err := l.Accept()
		if err != nil {
			log.Fatal(err)
		}
		log.Printf("accepted connection from %s\n", conn.RemoteAddr())

		// This should be optional in the blog post; the type assertion is done
		// only to invoke the ConnectionState() method.
		//if tlsconn, ok := conn.(*tls.Conn); ok {
		//log.Printf("%+v\n", tlsconn.ConnectionState())
		//}

		go func(c net.Conn) {
			io.Copy(c, c)
			c.Close()
			log.Printf("closing connection from %s\n", conn.RemoteAddr())
		}(conn)
	}
}

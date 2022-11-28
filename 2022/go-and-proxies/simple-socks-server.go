package main

import (
	"flag"
	"io"
	"log"
	"net"
)

func main() {
	port := flag.String("port", "1080", "port for listening to SOCKS5 traffic")
	flag.Parse()

	l, err := net.Listen("tcp", "localhost:"+*port)
	if err != nil {
		log.Fatal(err)
	}

	for {
		conn, err := l.Accept()
		if err != nil {
			log.Fatal(err)
		}
		go serveSOCKS5(conn)
	}
}

func serveSOCKS5(conn net.Conn) {
	defer conn.Close()

	version := make([]byte, 1)
	if _, err := conn.Read(version); err != nil {
		log.Fatal(err)
	}

	if int(version[0]) != 5 {
		log.Println("Unsupported SOCKS5 version: want 5, got", version)
		return
	}

	nmethods := make([]byte, 1)
	if _, err := conn.Read(nmethods); err != nil {
		log.Fatal(err)
	}

	methods := make([]byte, int(nmethods[0]))
	if _, err := io.ReadFull(conn, methods); err != nil {
		log.Fatal(err)
	}

	if _, err := conn.Write([]byte{5, 0}); err != nil {
		log.Fatal(err)
	}
}

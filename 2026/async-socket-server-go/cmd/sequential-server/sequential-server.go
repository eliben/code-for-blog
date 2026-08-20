// Basic sequential server that serves one connection at a time, in a loop.
//
// Eli Bendersky [https://eli.thegreenplace.net]
package main

import (
	"log"
	"net"
	"os"

	"example.com/server"
)

func main() {
	port := "9090"
	if len(os.Args) >= 2 {
		port = os.Args[1]
	}
	log.Println("Serving on port", port)

	listener, err := net.Listen("tcp", ":"+port)
	if err != nil {
		log.Fatal("Error listening:", err)
	}
	defer listener.Close()

	// Not using a WaitGroup here, because the server is designed to
	// run indefinitely, and there's nothing to wait for.
	for {
		conn, err := listener.Accept()
		if err != nil {
			log.Printf("Error accepting connection: %v", err)
			continue
		}
		log.Println("connection received from", conn.RemoteAddr())

		if err := server.ServeSerialProtocol(conn); err != nil {
			log.Printf("Error serving %v: %v", conn.RemoteAddr(), err)
		} else {
			log.Println("peer done", conn.RemoteAddr())
		}
	}
}

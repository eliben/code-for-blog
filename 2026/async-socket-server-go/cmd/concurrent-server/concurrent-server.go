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

	for {
		conn, err := listener.Accept()
		if err != nil {
			log.Printf("Error accepting connection: %v", err)
			continue
		}
		log.Println("connection received from", conn.RemoteAddr())

		go func() {
			if err := server.ServeSerialProtocol(conn); err != nil {
				log.Printf("Error serving %v: %v", conn.RemoteAddr(), err)
			} else {
				log.Println("peer done", conn.RemoteAddr())
			}
		}()
	}
}

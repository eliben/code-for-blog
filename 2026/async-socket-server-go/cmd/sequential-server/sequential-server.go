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

	listener, err := net.Listen("tcp", ":"+port)
	if err != nil {
		log.Fatal("Error listening:", err)
	}
	defer listener.Close()

	for {
		conn, err := listener.Accept()
		if err != nil {
			log.Println("Error accepting conn: err")
			continue
		}

		server.ServeSerialProtocol(conn)
	}
}

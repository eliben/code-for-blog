package main

import (
	"log"
	"net"
	"os"
	"sync"

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

	var wg sync.WaitGroup
	for {
		conn, err := listener.Accept()
		if err != nil {
			log.Printf("Error accepting connection: %v", err)
			continue
		}
		log.Println("connection received from", conn.RemoteAddr())

		wg.Add(1)
		go func(c net.Conn) {
			defer wg.Done()
			if err := server.ServeSerialProtocol(c); err != nil {
				log.Printf("Error serving %v: %v", c, err)
			}
			log.Println("peer done", c.RemoteAddr())
		}(conn)
	}

	wg.Wait()
}

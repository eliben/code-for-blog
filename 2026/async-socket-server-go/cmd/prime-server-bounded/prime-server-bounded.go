// Concurrent primes server - limits concurrency using a semaphore channel.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"log"
	"net"
	"os"
	"runtime"

	"example.com/server"
)

func main() {
	port := "8070"
	if len(os.Args) >= 2 {
		port = os.Args[1]
	}
	log.Println("Serving on port", port)

	listener, err := net.Listen("tcp", ":"+port)
	if err != nil {
		log.Fatal("Error listening:", err)
	}
	defer listener.Close()

	maxConcurrency := runtime.NumCPU()
	sem := make(chan struct{}, maxConcurrency)

	for {
		conn, err := listener.Accept()
		if err != nil {
			log.Printf("Error accepting connection: %v", err)
			continue
		}
		log.Println("connection received from", conn.RemoteAddr())

		// Acquire a token from the semaphore to limit concurrency.
		sem <- struct{}{}

		go func() {
			// Return the token when done serving the connection.
			defer func() { <-sem }()

			if err := server.ServePrimeProtocol(conn); err != nil {
				log.Printf("Error serving %v: %v", conn.RemoteAddr(), err)
			} else {
				log.Println("peer done", conn.RemoteAddr())
			}
		}()
	}
}

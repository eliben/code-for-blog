package main

import (
	"log"
	"net"
	"os"
	"runtime"

	"example.com/server"
)

func worker(jobs <-chan net.Conn) {
	for conn := range jobs {
		if err := server.ServePrimeProtocol(conn); err != nil {
			log.Printf("Error serving %v: %v", conn.RemoteAddr(), err)
		} else {
			log.Println("peer done", conn.RemoteAddr())
		}
	}
}

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

	jobs := make(chan net.Conn)
	maxConcurrency := runtime.NumCPU()
	for i := 0; i < maxConcurrency; i++ {
		go worker(jobs)
	}

	for {
		conn, err := listener.Accept()
		if err != nil {
			log.Printf("Error accepting connection: %v", err)
			continue
		}
		log.Println("connection received from", conn.RemoteAddr())

		jobs <- conn
	}
}

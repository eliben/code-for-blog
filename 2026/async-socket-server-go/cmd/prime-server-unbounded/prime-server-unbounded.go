package main

import (
	"log"
	"net"
	"os"
	"strconv"
	"strings"

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

	for {
		conn, err := listener.Accept()
		if err != nil {
			log.Printf("Error accepting connection: %v", err)
			continue
		}
		log.Println("connection received from", conn.RemoteAddr())

		go func() {
			if err := serveClient(conn); err != nil {
				log.Printf("Error serving %v: %v", conn.RemoteAddr(), err)
			}
			log.Println("peer done", conn.RemoteAddr())
		}()
	}
}

func serveClient(conn net.Conn) error {
	defer conn.Close()
	buf := make([]byte, 1024)

	for {
		n, readerr := conn.Read(buf)
		if n == 0 {
			return nil
		}

		// Parse the read buffer to an i64
		num, err := strconv.ParseInt(strings.TrimSpace(string(buf[:n])), 10, 64)
		if err != nil {
			return err
		}

		var response string
		if server.IsPrime(num, true) {
			response = "prime"
		} else {
			response = "composite"
		}
		if _, err := conn.Write([]byte(response + "\n")); err != nil {
			return err
		}

		if readerr != nil {
			return readerr
		}
	}
}

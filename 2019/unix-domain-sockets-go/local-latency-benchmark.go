// Latency benchmark for comparing Unix sockets with TCP sockets.
//
// Idea: ping-pong 128-byte packets between a goroutine acting as a server and
// main acting as client. Measure how long it took to do 2*N ping-pongs and find
// the average latency.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"flag"
	"fmt"
	"log"
	"net"
	"os"
	"time"
)

var UnixDomain = flag.Bool("unixdomain", false, "Use Unix domain sockets")
var MsgSize = flag.Int("msgsize", 128, "Message size in each ping")
var NumPings = flag.Int("n", 50000, "Number of pings to measure")

var TcpAddress = "127.0.0.1:13500"
var UnixAddress = "/tmp/benchmark.sock"

// domainAndAddress returns the domain,address pair for net functions to connect
// to, depending on the value of the UnixDomain flag.
func domainAndAddress() (string, string) {
	if *UnixDomain {
		return "unix", UnixAddress
	} else {
		return "tcp", TcpAddress
	}
}

func server() {
	if *UnixDomain {
		if err := os.RemoveAll(UnixAddress); err != nil {
			panic(err)
		}
	}

	domain, address := domainAndAddress()
	l, err := net.Listen(domain, address)
	if err != nil {
		log.Fatal(err)
	}
	defer l.Close()

	conn, err := l.Accept()
	if err != nil {
		log.Fatal(err)
	}
	defer conn.Close()

	buf := make([]byte, *MsgSize)
	for n := 0; n < *NumPings; n++ {
		nread, err := conn.Read(buf)
		if err != nil {
			log.Fatal(err)
		}
		if nread != *MsgSize {
			log.Fatalf("bad nread = %d", nread)
		}
		nwrite, err := conn.Write(buf)
		if err != nil {
			log.Fatal(err)
		}
		if nwrite != *MsgSize {
			log.Fatalf("bad nwrite = %d", nwrite)
		}
	}

	time.Sleep(50 * time.Millisecond)
}

func main() {
	flag.Parse()

	go server()
	time.Sleep(50 * time.Millisecond)

	// This is the client code in the main goroutine.
	domain, address := domainAndAddress()
	conn, err := net.Dial(domain, address)
	if err != nil {
		log.Fatal(err)
	}

	buf := make([]byte, *MsgSize)
	t1 := time.Now()
	for n := 0; n < *NumPings; n++ {
		nwrite, err := conn.Write(buf)
		if err != nil {
			log.Fatal(err)
		}
		if nwrite != *MsgSize {
			log.Fatalf("bad nwrite = %d", nwrite)
		}
		nread, err := conn.Read(buf)
		if err != nil {
			log.Fatal(err)
		}
		if nread != *MsgSize {
			log.Fatalf("bad nread = %d", nread)
		}
	}
	elapsed := time.Since(t1)

	totalpings := int64(*NumPings * 2)
	fmt.Println("Client done")
	fmt.Printf("%d pingpongs took %d ns; avg. latency %d ns\n",
		totalpings, elapsed.Nanoseconds(),
		elapsed.Nanoseconds()/totalpings)

	time.Sleep(50 * time.Millisecond)
}

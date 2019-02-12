// Throughput benchmark for Unix sockets with TCP sockets.
//
// Sends large packets from client to server and measures how long each send
// took.
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

var TcpAddress = "127.0.0.1:13500"
var UnixAddress = "/tmp/benchmark.sock"

var UnixDomain = flag.Bool("unixdomain", false, "Use Unix domain sockets")
var MsgSize = flag.Int("msgsize", 256*1024, "Size of each message")
var NumMsg = flag.Int("n", 10000, "Number of messages to send")

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
	for {
		nread, err := conn.Read(buf)
		if err != nil {
			log.Fatal(err)
		}
		if nread == 0 {
			break
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
	for n := 0; n < *NumMsg; n++ {
		nwrite, err := conn.Write(buf)
		if err != nil {
			log.Fatal(err)
		}
		if nwrite != *MsgSize {
			log.Fatalf("bad nwrite = %d", nwrite)
		}
	}
	elapsed := time.Since(t1)

	totaldata := int64(*NumMsg * *MsgSize)
	fmt.Println("Client done")
	fmt.Printf("Sent %d msg in %d ns; throughput %d msg/sec (%d MB/sec)\n",
		*NumMsg, elapsed,
		(int64(*NumMsg)*1000000000)/elapsed.Nanoseconds(),
		(totaldata*1000)/elapsed.Nanoseconds())

	time.Sleep(50 * time.Millisecond)
}

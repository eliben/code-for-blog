// Run "go test shutdown1.go shutdown1_test.go"
package shutdown1

import (
	"fmt"
	"log"
	"math/rand"
	"net"
	"testing"
	"time"
)

func fastclient(addr string) {
	conn, err := net.Dial("tcp", addr)
	if err != nil {
		log.Fatal(err)
	}
	defer conn.Close()

	fmt.Fprintf(conn, "hello there")
	conn.Close()
}

func slowclient(addr string) {
	conn, err := net.Dial("tcp", addr)
	if err != nil {
		log.Fatal(err)
	}
	defer conn.Close()

	for i := 0; i < 3; i++ {
		fmt.Fprintf(conn, "hello %d from slow client", i)
		delay := 30 + rand.Intn(40)
		time.Sleep(time.Duration(delay) * time.Millisecond)
	}
}

const defaultAddr = "localhost:16161"

func TestServerStartStop(t *testing.T) {
	s := NewServer(defaultAddr)
	s.Stop()
}

func TestServerSimpleHandleFastClients(t *testing.T) {
	s := NewServer(defaultAddr)

	go fastclient(defaultAddr)
	go fastclient(defaultAddr)

	time.Sleep(100 * time.Millisecond)
	s.Stop()
}

func TestCantConnectAfterStop(t *testing.T) {
	s := NewServer(defaultAddr)
	go fastclient(defaultAddr)
	time.Sleep(100 * time.Millisecond)
	s.Stop()

	_, err := net.Dial("tcp", defaultAddr)
	if err == nil {
		t.Errorf("expected connection error")
	}
}

func TestSlowClients(t *testing.T) {
	s := NewServer(defaultAddr)

	go slowclient(defaultAddr)
	go slowclient(defaultAddr)
	go slowclient(defaultAddr)

	time.Sleep(250 * time.Millisecond)
	s.Stop()
}

func TestSlowClientEndingAfterStop(t *testing.T) {
	// Here the slow client finishes after we called s.Stop; everything should
	// still shut down in an orderly way.
	s := NewServer(defaultAddr)

	go slowclient(defaultAddr)

	// The slow client delays 90ms at the minimum, so we set the Sleep to before
	// that.
	time.Sleep(50 * time.Millisecond)
	s.Stop()
}

func TestMultipleServers(t *testing.T) {
	const addr1 = "localhost:17771"
	const addr2 = "localhost:17772"

	s1 := NewServer(addr1)
	s2 := NewServer(addr2)

	go slowclient(addr1)
	go slowclient(addr1)
	go slowclient(addr2)
	go slowclient(addr2)

	time.Sleep(250 * time.Millisecond)
	s1.Stop()
	s2.Stop()

	_, err := net.Dial("tcp", addr1)
	if err == nil {
		t.Errorf("expected connection error")
	}

	_, err = net.Dial("tcp", addr2)
	if err == nil {
		t.Errorf("expected connection error")
	}
}

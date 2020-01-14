package shutdown2

import (
	"fmt"
	"io"
	"log"
	"math/rand"
	"net"
	"testing"
	"time"
)

func fastclient(addr net.Addr) {
	conn, err := net.Dial(addr.Network(), addr.String())
	if err != nil {
		log.Fatal(err)
	}
	defer conn.Close()

	fmt.Fprintf(conn, "hello there")
	conn.Close()
}

func slowclient(addr net.Addr) {
	conn, err := net.Dial(addr.Network(), addr.String())
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

func wantFailDial(t *testing.T, addr net.Addr) {
	_, err := net.Dial(addr.Network(), addr.String())
	if err == nil {
		t.Errorf("expected connection error")
	}
}

func TestServerStartStop(t *testing.T) {
	s := NewServer(":0")
	s.Stop()
}

func TestServerSimpleHandleFastClients(t *testing.T) {
	s := NewServer(":0")

	go fastclient(s.listener.Addr())
	go fastclient(s.listener.Addr())

	time.Sleep(100 * time.Millisecond)
	s.Stop()
}

func TestCantConnectAfterStop(t *testing.T) {
	s := NewServer(":0")
	go fastclient(s.listener.Addr())
	time.Sleep(100 * time.Millisecond)
	s.Stop()

	wantFailDial(t, s.listener.Addr())
}

func TestSlowClients(t *testing.T) {
	s := NewServer(":0")

	go slowclient(s.listener.Addr())
	go slowclient(s.listener.Addr())
	go slowclient(s.listener.Addr())

	time.Sleep(250 * time.Millisecond)
	s.Stop()
}

func TestSlowClientEndingAfterStop(t *testing.T) {
	// Here the slow client finishes after we called s.Stop; everything should
	// still shut down in an orderly way.
	s := NewServer(":0")

	go slowclient(s.listener.Addr())

	// The slow client delays 90ms at the minimum, so we set the Sleep to before
	// that.
	time.Sleep(50 * time.Millisecond)
	s.Stop()
	wantFailDial(t, s.listener.Addr())
}

func TestMultipleServers(t *testing.T) {
	s1 := NewServer(":0")
	s2 := NewServer(":0")

	go slowclient(s1.listener.Addr())
	go slowclient(s1.listener.Addr())
	go slowclient(s2.listener.Addr())
	go slowclient(s2.listener.Addr())

	time.Sleep(250 * time.Millisecond)
	s1.Stop()
	s2.Stop()

	wantFailDial(t, s1.listener.Addr())
	wantFailDial(t, s2.listener.Addr())
}

func TestVerySlowClientCutoff(t *testing.T) {
	s := NewServer(":0")

	client := func() {
		conn, err := net.Dial(s.listener.Addr().Network(), s.listener.Addr().String())
		if err != nil {
			log.Fatal(err)
		}
		defer conn.Close()

		time.Sleep(350 * time.Millisecond)
		fmt.Fprintf(conn, "i'm slow")

		// Expect this to fail with an EOF because the server has closed
		// the connection.
		buf := make([]byte, 2048)
		_, err = conn.Read(buf)

		if err != io.EOF {
			t.Errorf("want error = io.EOF, got %v", err)
		}
	}
	go client()

	time.Sleep(50 * time.Millisecond)
	s.Stop()

	wantFailDial(t, s.listener.Addr())
	time.Sleep(350 * time.Millisecond)
}

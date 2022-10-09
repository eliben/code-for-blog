// Sample client to test the go websocket server.
//
// Runs some sanity checks against a server, and log.Fatal-s in case of errors.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"flag"
	"fmt"
	"log"
	"time"

	"golang.org/x/net/websocket"
)

var (
	// Port number where the server listens
	serverport = flag.Int("serverport", 4050, "The server port")
)

type Event struct {
	X int `json:"x"`
	Y int `json:"y"`
}

var base_addr = "ws://localhost"
var origin = "http://localhost"

// checkWsEcho checks that the WebSocket echo server route functions correctly.
func checkWsEcho() {
	addr := fmt.Sprintf("%s:%d/wsecho", base_addr, *serverport)
	conn, err := websocket.Dial(addr, "", origin)
	if err != nil {
		log.Fatal("websocket.Dial error", err)
	}
	e := Event{
		X: 42,
		Y: 123456,
	}
	err = websocket.JSON.Send(conn, e)
	if err != nil {
		log.Fatal("websocket.JSON.Send error", err)
	}

	var reply Event
	err = websocket.JSON.Receive(conn, &reply)
	if err != nil {
		log.Fatal("websocket.JSON.Receive error", err)
	}

	if reply != e {
		log.Fatalf("reply != e: %s != %s", reply, e)
	}
	if err = conn.Close(); err != nil {
		log.Fatal("conn.Close error", err)
	}
}

// verifyTimeMessage verifies that msg is a valid time string from the server.
func verifyTimeMessage(msg string) {
	_, err := time.Parse("Mon, 02 Jan 2006 15:04:05 PST", msg)
	if err != nil {
		log.Fatal("message time parse error", err)
	}
}

// checkWsTime checks that the WebSocket time server route functions correctly.
func checkWsTime() {
	addr := fmt.Sprintf("%s:%d/wstime", base_addr, *serverport)
	conn, err := websocket.Dial(addr, "", origin)
	if err != nil {
		log.Fatal("websocket.Dial error", err)
	}

	c := make(chan string)
	go func() {
		var msg string
		if err := websocket.Message.Receive(conn, &msg); err == nil {
			c <- msg
		} else {
			log.Fatal("websocket.Message.Receive error", err)
		}
	}()

	select {
	case msg := <-c:
		verifyTimeMessage(msg)
	case <-time.After(time.Millisecond * 1200):
		log.Fatal("Didn't receive time message within timeout")
	}

	if err = conn.Close(); err != nil {
		log.Fatal("conn.Close error", err)
	}
}

func main() {
	flag.Parse()
	checkWsEcho()
	checkWsTime()
	log.Println("Success")
}

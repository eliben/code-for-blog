// Server-side part of the Go websocket sample.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"flag"
	"fmt"
	"log"
	"net/http"
	"time"

	"golang.org/x/net/trace"
	"golang.org/x/net/websocket"
)

var (
	port = flag.Int("port", 4050, "The server port")
)

type Event struct {
	// The fields of this struct must be exported so that the json module will be
	// able to write into them. Therefore we need field tags to specify the names
	// by which these fields go in the JSON representation of events.
	X int `json:"x"`
	Y int `json:"y"`
}

// handleWebsocketEchoMessage handles the message e arriving on connection ws
// from the client.
func handleWebsocketEchoMessage(ws *websocket.Conn, e Event) error {
	// Log the request with net.Trace
	tr := trace.New("websocket.Receive", "receive")
	defer tr.Finish()
	tr.LazyPrintf("Got event %v\n", e)

	// Echo the event back as JSON
	err := websocket.JSON.Send(ws, e)
	if err != nil {
		return fmt.Errorf("Can't send: %s", err.Error())
	}
	return nil
}

// websocketEchoConnection handles a single websocket echo connection - ws.
func websocketEchoConnection(ws *websocket.Conn) {
	log.Printf("Client connected from %s", ws.RemoteAddr())
	for {
		var event Event
		err := websocket.JSON.Receive(ws, &event)
		if err != nil {
			log.Printf("Receive failed: %s; closing connection...", err.Error())
			if err = ws.Close(); err != nil {
				log.Println("Error closing connection:", err.Error())
			}
			break
		} else {
			if err := handleWebsocketEchoMessage(ws, event); err != nil {
				log.Println(err.Error())
				break
			}
		}
	}
}

// websocketTimeConnection handles a single websocket time connection - ws.
func websocketTimeConnection(ws *websocket.Conn) {
	for range time.Tick(1 * time.Second) {
		// Once a second, send a message (as a string) with the current time.
		websocket.Message.Send(ws, time.Now().Format("Mon, 02 Jan 2006 15:04:05 PST"))
	}
}

func main() {
	flag.Parse()
	// Set up websocket servers and static file server. In addition, we're using
	// net/trace for debugging - it will be available at /debug/requests.
	http.Handle("/wsecho", websocket.Handler(websocketEchoConnection))
	http.Handle("/wstime", websocket.Handler(websocketTimeConnection))
	http.Handle("/", http.FileServer(http.Dir("static/html")))

	log.Printf("Server listening on port %d", *port)
	log.Fatal(http.ListenAndServe(fmt.Sprintf(":%d", *port), nil))
}

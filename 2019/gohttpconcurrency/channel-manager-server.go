// This version of the server protects all shared data within a manager
// goroutine that accepts commands using a channel.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"fmt"
	"log"
	"net/http"
	"os"
	"strconv"
)

type CommandType int

const (
	GetCommand = iota
	SetCommand
	IncCommand
)

type Command struct {
	ty        CommandType
	name      string
	val       int
	replyChan chan int
}

// startCounterManager starts a goroutine that serves as a manager for our
// counters datastore. Returns a channel that's used to send commands to the
// manager.
func startCounterManager(initvals map[string]int) chan<- Command {
	counters := make(map[string]int)
	for k, v := range initvals {
		counters[k] = v
	}

	cmds := make(chan Command)

	go func() {
		for cmd := range cmds {
			switch cmd.ty {
			case GetCommand:
				if val, ok := counters[cmd.name]; ok {
					cmd.replyChan <- val
				} else {
					cmd.replyChan <- -1
				}
			case SetCommand:
				counters[cmd.name] = cmd.val
				cmd.replyChan <- cmd.val
			case IncCommand:
				if _, ok := counters[cmd.name]; ok {
					counters[cmd.name]++
					cmd.replyChan <- counters[cmd.name]
				} else {
					cmd.replyChan <- -1
				}
			default:
				log.Fatal("unknown command type", cmd.ty)
			}
		}
	}()
	return cmds
}

// Server is the shared data structure for HTTP handlers. It wraps a channel of
// commands that are used to interact with a manager running concurrently.
type Server struct {
	cmds chan<- Command
}

func (s *Server) get(w http.ResponseWriter, req *http.Request) {
	log.Printf("get %v", req)
	name := req.URL.Query().Get("name")
	replyChan := make(chan int)
	s.cmds <- Command{ty: GetCommand, name: name, replyChan: replyChan}
	reply := <-replyChan

	if reply >= 0 {
		fmt.Fprintf(w, "%s: %d\n", name, reply)
	} else {
		fmt.Fprintf(w, "%s not found\n", name)
	}
}

func (s *Server) set(w http.ResponseWriter, req *http.Request) {
	log.Printf("set %v", req)
	name := req.URL.Query().Get("name")
	val := req.URL.Query().Get("val")
	intval, err := strconv.Atoi(val)
	if err != nil {
		fmt.Fprintf(w, "%s\n", err)
	} else {
		replyChan := make(chan int)
		s.cmds <- Command{ty: SetCommand, name: name, val: intval, replyChan: replyChan}
		_ = <-replyChan
		fmt.Fprintf(w, "ok\n")
	}
}

func (s *Server) inc(w http.ResponseWriter, req *http.Request) {
	log.Printf("inc %v", req)
	name := req.URL.Query().Get("name")
	replyChan := make(chan int)
	s.cmds <- Command{ty: IncCommand, name: name, replyChan: replyChan}

	reply := <-replyChan
	if reply >= 0 {
		fmt.Fprintf(w, "ok\n")
	} else {
		fmt.Fprintf(w, "%s not found\n", name)
	}
}

func main() {
	server := Server{startCounterManager(map[string]int{"i": 0, "j": 0})}
	http.HandleFunc("/get", server.get)
	http.HandleFunc("/set", server.set)
	http.HandleFunc("/inc", server.inc)

	portnum := 8000
	if len(os.Args) > 1 {
		portnum, _ = strconv.Atoi(os.Args[1])
	}
	log.Printf("Going to listen on port %d\n", portnum)
	log.Fatal(http.ListenAndServe("localhost:"+strconv.Itoa(portnum), nil))
}

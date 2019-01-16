// Rate-limited version of the mutex server
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
	"sync"
)

type CounterStore struct {
	sync.Mutex
	counters map[string]int
}

func (cs *CounterStore) get(w http.ResponseWriter, req *http.Request) {
	log.Printf("get %v", req)
	cs.Lock()
	defer cs.Unlock()
	name := req.URL.Query().Get("name")
	if val, ok := cs.counters[name]; ok {
		fmt.Fprintf(w, "%s: %d\n", name, val)
	} else {
		fmt.Fprintf(w, "%s not found\n", name)
	}
}

func (cs *CounterStore) set(w http.ResponseWriter, req *http.Request) {
	log.Printf("set %v", req)
	cs.Lock()
	defer cs.Unlock()
	name := req.URL.Query().Get("name")
	val := req.URL.Query().Get("val")
	intval, err := strconv.Atoi(val)
	if err != nil {
		fmt.Fprintf(w, "%s\n", err)
	} else {
		cs.counters[name] = intval
		fmt.Fprintf(w, "ok\n")
	}
}

// limitNumClients is HTTP handling middleware that ensures no more than
// maxClients requests are passed concurrently to the given handler f.
func limitNumClients(f http.HandlerFunc, maxClients int) http.HandlerFunc {
	sema := make(chan struct{}, maxClients)

	return func(w http.ResponseWriter, req *http.Request) {
		sema <- struct{}{}
		defer func() { <-sema }()
		f(w, req)
	}
}

func (cs *CounterStore) inc(w http.ResponseWriter, req *http.Request) {
	log.Printf("inc %v", req)
	cs.Lock()
	defer cs.Unlock()
	name := req.URL.Query().Get("name")
	if _, ok := cs.counters[name]; ok {
		cs.counters[name]++
		fmt.Fprintf(w, "ok\n")
	} else {
		fmt.Fprintf(w, "%s not found\n", name)
	}
}

func main() {
	store := CounterStore{counters: map[string]int{"i": 0, "j": 0}}
	http.HandleFunc("/get", store.get)
	http.HandleFunc("/set", store.set)

	// Limit to max 10 connections for this handler.
	http.HandleFunc("/inc", limitNumClients(store.inc, 10))

	portnum := 8000
	if len(os.Args) > 1 {
		portnum, _ = strconv.Atoi(os.Args[1])
	}
	log.Printf("Going to listen on port %d\n", portnum)
	log.Fatal(http.ListenAndServe("localhost:"+strconv.Itoa(portnum), nil))
}

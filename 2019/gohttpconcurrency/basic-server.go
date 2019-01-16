// This is a basic server that has race conditions. The code is flawed - its
// only use is to showcase the bugs.
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

// CounterStore is the datastore for this application; it contains a collection
// of counters, keyed by name. Each counter has an integer value.
type CounterStore struct {
	counters map[string]int
}

func (cs CounterStore) get(w http.ResponseWriter, req *http.Request) {
	log.Printf("get %v", req)
	name := req.URL.Query().Get("name")
	if val, ok := cs.counters[name]; ok {
		fmt.Fprintf(w, "%s: %d\n", name, val)
	} else {
		fmt.Fprintf(w, "%s not found\n", name)
	}
}

func (cs CounterStore) set(w http.ResponseWriter, req *http.Request) {
	log.Printf("set %v", req)
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

func (cs CounterStore) inc(w http.ResponseWriter, req *http.Request) {
	log.Printf("inc %v", req)
	name := req.URL.Query().Get("name")
	if _, ok := cs.counters[name]; ok {
		cs.counters[name]++
		fmt.Fprintf(w, "ok\n")
	} else {
		fmt.Fprintf(w, "%s not found\n", name)
	}
}

func main() {
	// Create the store and seed it with a couple of pre-defined counters.
	store := CounterStore{counters: map[string]int{"i": 0, "j": 0}}
	http.HandleFunc("/get", store.get)
	http.HandleFunc("/set", store.set)
	http.HandleFunc("/inc", store.inc)

	portnum := 8000
	if len(os.Args) > 1 {
		portnum, _ = strconv.Atoi(os.Args[1])
	}
	log.Printf("Going to listen on port %d\n", portnum)
	log.Fatal(http.ListenAndServe("localhost:"+strconv.Itoa(portnum), nil))
}

package main

import (
	"fmt"
	"log"
	"net/http"
	"time"
)

func timeHandler(w http.ResponseWriter, r *http.Request) {
	fmt.Fprint(w, time.Now().Format("02 Jan 2006 15:04:05 MST"))
}

func main() {
	http.HandleFunc("/time", timeHandler)
	http.Handle("/", http.FileServer(http.Dir("public/")))

	port := ":9999"
	log.Fatal(http.ListenAndServe(port, nil))
}

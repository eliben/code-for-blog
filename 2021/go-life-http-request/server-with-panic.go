package main

import (
	"fmt"
	"net/http"
)

func hello(w http.ResponseWriter, req *http.Request) {
	fmt.Fprintf(w, "hello\n")
}

func doPanic(w http.ResponseWriter, req *http.Request) {
	panic("oops")
}

func main() {
	http.HandleFunc("/hello", hello)
	http.HandleFunc("/panic", doPanic)

	http.ListenAndServe(":8090", nil)
}

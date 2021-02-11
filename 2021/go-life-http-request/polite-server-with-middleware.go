package main

import (
	"fmt"
	"log"
	"net/http"
	"time"
)

type LoggingMiddleware struct {
	handler http.Handler
}

func (lm *LoggingMiddleware) ServeHTTP(w http.ResponseWriter, req *http.Request) {
	start := time.Now()
	lm.handler.ServeHTTP(w, req)
	log.Printf("%s %s %s", req.Method, req.RequestURI, time.Since(start))
}

type PoliteServer struct {
}

func (ms *PoliteServer) ServeHTTP(w http.ResponseWriter, req *http.Request) {
	fmt.Fprintf(w, "Welcome! Thanks for visiting!\n")
}

func main() {
	ps := &PoliteServer{}
	lm := &LoggingMiddleware{handler: ps}
	log.Fatal(http.ListenAndServe(":8090", lm))
}

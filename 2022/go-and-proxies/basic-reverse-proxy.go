package main

import (
	"flag"
	"log"
	"net/http"
	"net/http/httputil"
	"net/url"
	"strings"
)

func main() {
	fromAddr := flag.String("from", "127.0.0.1:9090", "proxy's listening address")
	toAddr := flag.String("to", "127.0.0.1:8080", "the address this proxy will forward to")
	flag.Parse()

	if !strings.HasPrefix(*toAddr, "http") {
		*toAddr = "http://" + *toAddr
	}
	toUrl, err := url.Parse(*toAddr)
	if err != nil {
		log.Fatal(err)
	}

	proxy := httputil.NewSingleHostReverseProxy(toUrl)
	log.Println("Starting proxy server on", *fromAddr)
	if err := http.ListenAndServe(*fromAddr, proxy); err != nil {
		log.Fatal("ListenAndServe:", err)
	}
}

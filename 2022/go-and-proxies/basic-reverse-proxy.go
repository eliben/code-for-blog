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

	toUrl := parseToUrl(*toAddr)
	proxy := httputil.NewSingleHostReverseProxy(toUrl)
	log.Println("Starting proxy server on", *fromAddr)
	if err := http.ListenAndServe(*fromAddr, proxy); err != nil {
		log.Fatal("ListenAndServe:", err)
	}
}

// parseToUrl parses a "to" address to url.URL value
func parseToUrl(addr string) *url.URL {
	if !strings.HasPrefix(addr, "http") {
		addr = "http://" + addr
	}
	toUrl, err := url.Parse(addr)
	if err != nil {
		log.Fatal(err)
	}
	return toUrl
}

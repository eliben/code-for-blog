// Basic forward proxy set up using httputil.ReverseProxy
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"flag"
	"log"
	"net/http"
	"net/http/httputil"
	"net/url"
)

func proxyHandler(w http.ResponseWriter, r *http.Request) {
	target, err := url.Parse(r.URL.Scheme + "://" + r.URL.Host)
	if err != nil {
		log.Fatal(err)
	}

	reqb, err := httputil.DumpRequest(r, true)
	if err != nil {
		log.Fatal(err)
	}
	log.Println(string(reqb))

	p := httputil.NewSingleHostReverseProxy(target)
	p.ServeHTTP(w, r)
}

func main() {
	var addr = flag.String("addr", "127.0.0.1:9999", "proxy address")
	flag.Parse()

	http.HandleFunc("/", proxyHandler)
	log.Println("Starting proxy server on", *addr)
	if err := http.ListenAndServe(*addr, nil); err != nil {
		log.Fatal("ListenAndServe:", err)
	}
}

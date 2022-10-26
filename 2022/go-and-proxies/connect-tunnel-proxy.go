package main

import (
	"flag"
	"io"
	"log"
	"net"
	"net/http"
)

type forwardProxy struct {
}

func (p *forwardProxy) ServeHTTP(w http.ResponseWriter, req *http.Request) {
	if req.Method == http.MethodConnect {
		proxyConnect(w, req)
	} else {
		log.Println("TODO: HTTP proxying not implemented")
		http.Error(w, "this proxy only supports CONNECT", http.StatusMethodNotAllowed)
	}
}

func proxyConnect(w http.ResponseWriter, req *http.Request) {
	log.Printf("CONNECT requested to %v (from %v)", req.Host, req.RemoteAddr)
	targetConn, err := net.Dial("tcp", req.Host)
	if err != nil {
		log.Println("failed to dial to target", req.Host)
		http.Error(w, err.Error(), http.StatusServiceUnavailable)
		return
	}

	w.WriteHeader(http.StatusOK)
	hj, ok := w.(http.Hijacker)
	if !ok {
		log.Fatal("http server doesn't support hijacking connection")
	}

	clientConn, _, err := hj.Hijack()
	if err != nil {
		log.Fatal("http hijacking failed")
	}

	log.Println("tunnel established")
	go tunnelConn(targetConn, clientConn)
	go tunnelConn(clientConn, targetConn)
}

func tunnelConn(dst io.WriteCloser, src io.ReadCloser) {
	io.Copy(dst, src)
	dst.Close()
	src.Close()
}

func main() {
	var addr = flag.String("addr", "127.0.0.1:9999", "proxy address")
	flag.Parse()

	proxy := &forwardProxy{}

	log.Println("Starting proxy server on", *addr)
	if err := http.ListenAndServe(*addr, proxy); err != nil {
		log.Fatal("ListenAndServe:", err)
	}
}

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

func (p *forwardProxy) ServeHTTP(wr http.ResponseWriter, req *http.Request) {
	if req.Method == http.MethodConnect {
		proxyConnect(wr, req)
	} else {
		log.Println("TODO: HTTP proxying not implemented")
		http.Error(wr, "this proxy only supports CONNECT", http.StatusMethodNotAllowed)
	}
}

func proxyConnect(wr http.ResponseWriter, req *http.Request) {
	log.Printf("CONNECT requested to %v (from %v)", req.Host, req.RemoteAddr)
	targetConn, err := net.Dial("tcp", req.Host)
	if err != nil {
		log.Println("failed to dial to target", req.Host)
		http.Error(wr, err.Error(), http.StatusServiceUnavailable)
		return
	}

	wr.WriteHeader(http.StatusOK)
	hj, ok := wr.(http.Hijacker)
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

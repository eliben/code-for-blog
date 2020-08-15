// Embed interface in struct to catch common behavior and redirect the
// rest.
// Shows https://github.com/golang/go/issues/22013#issuecomment-331886875 in
// action.
package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"net"
	"net/url"
	"os"
)

type StatsConn struct {
	net.Conn

	BytesRead uint64
}

func (sc *StatsConn) Read(p []byte) (int, error) {
	n, err := sc.Conn.Read(p)
	sc.BytesRead += uint64(n)
	return n, err
}

// Run with host as argument
func main() {
	s := os.Args[1]
	u, err := url.Parse(s)
	if err != nil {
		log.Fatal(err)
	}

	conn, err := net.Dial("tcp", u.Host+":80")
	if err != nil {
		log.Fatal(err)
	}
	sconn := &StatsConn{conn, 0}

	rt := fmt.Sprintf("GET %v HTTP/1.1\r\n", u.Path)
	rt += fmt.Sprintf("Host: %v\r\n", u.Host)
	rt += fmt.Sprintf("Connection: close\r\n")
	rt += fmt.Sprintf("\r\n")

	_, err = sconn.Write([]byte(rt))
	if err != nil {
		log.Fatal(err)
	}

	resp, err := ioutil.ReadAll(sconn)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println(string(resp))

	sconn.Close()

	fmt.Println("BytesRead:", sconn.BytesRead)
}

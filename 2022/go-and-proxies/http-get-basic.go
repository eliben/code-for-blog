// A basic HTTP client that issues a GET request to the address given with the
// --target flag. Can be used to explore how the proxy-setting env vars
// (like HTTP_PROXY) affect the default Go HTTP client.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
)

func main() {
	target := flag.String("target", "http://example.org", "URL to get")
	flag.Parse()

	resp, err := http.Get(*target)
	if err != nil {
		log.Fatal(err)
	}
	defer resp.Body.Close()

	fmt.Println("Response status:", resp.Status)
	fmt.Println("Content length:", resp.ContentLength)
	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println(string(body))
}

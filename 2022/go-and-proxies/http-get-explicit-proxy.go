package main

import (
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"net/url"
)

func main() {
	target := flag.String("target", "http://example.org", "URL to get")
	proxy := flag.String("proxy", "http://localhost:9999", "proxy to use")
	flag.Parse()

	proxyUrl, err := url.Parse(*proxy)
	if err != nil {
		log.Fatal(err)
	}
	client := &http.Client{
		Transport: &http.Transport{Proxy: http.ProxyURL(proxyUrl)},
	}

	r, err := client.Get(*target)
	if err != nil {
		log.Fatal(err)
	}
	defer r.Body.Close()
	body, err := ioutil.ReadAll(r.Body)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println(string(body))
}

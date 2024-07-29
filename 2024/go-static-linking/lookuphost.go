package main

import (
	"fmt"
	"net"
)

func main() {
	fmt.Println(net.LookupHost("google.com"))
}

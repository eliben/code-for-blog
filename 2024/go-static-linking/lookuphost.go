package main

import (
	"fmt"
	"net"
)

func main() {
	fmt.Println(net.LookupHost("go.dev"))
}

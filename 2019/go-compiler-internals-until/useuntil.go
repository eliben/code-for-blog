package main

import "fmt"

func useuntil() {
	i := 4
	until i == 0 {
		i--
		sayhi()
	}
}

func sayhi() {
	fmt.Println("Hello, for!")
}

func main() {
	useuntil()
}

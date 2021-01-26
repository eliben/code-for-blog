package main

import "fmt"

func ReverseInts(s []int) {
	first := 0
	last := len(s) - 1
	for first < last {
		s[first], s[last] = s[last], s[first]
		first++
		last--
	}
}

func ReverseStrings(s []string) {
	first := 0
	last := len(s) - 1
	for first < last {
		s[first], s[last] = s[last], s[first]
		first++
		last--
	}
}

func main() {
	s := []int{2, 4, 8, 11}
	ReverseInts(s)
	fmt.Println(s)

	ss := []string{"joe", "mike", "hello"}
	ReverseStrings(ss)
	fmt.Println(ss)
}

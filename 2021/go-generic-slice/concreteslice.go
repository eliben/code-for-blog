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

func ReverseAnything(s []interface{}) {
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
	//ReverseStrings(ss)
	//fmt.Println(ss)

	//ReverseAnything(ss)
	iis := make([]interface{}, len(ss))
	for i, s := range ss {
		iis[i] = s
	}

	ReverseAnything(iis)
	fmt.Println(iis)

	iints := []interface{}{2, 3, 4, 5}
	ReverseAnything(iints)
	fmt.Println(iints)

	istrings := []interface{}{"joe", "mike", "hello"}
	ReverseAnything(istrings)
	fmt.Println(istrings)
}

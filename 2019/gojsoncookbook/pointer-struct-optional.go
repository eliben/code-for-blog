package main

import (
	"encoding/json"
	"fmt"
)

type RequestBodyFoo struct {
	Name    string
	Balance float64
}

type RequestBodyBar struct {
	Id  int
	Ref int
}

type Request struct {
	Foo *RequestBodyFoo
	Bar *RequestBodyBar
}

func (r *Request) Show() {
	if r.Foo != nil {
		fmt.Println("Request has Foo:", *r.Foo)
	}
	if r.Bar != nil {
		fmt.Println("Request has Bar:", *r.Bar)
	}
}

func main() {
	bb := []byte(`
	{
		"Foo": {"Name": "joe", "balance": 4591.25}
	}
	`)

	var req Request
	if err := json.Unmarshal(bb, &req); err != nil {
		panic(err)
	}
	req.Show()

	var req2 Request
	bb = []byte(`
	{
		"Bar": {"Id": 128992, "Ref": 801472}
	}
	`)
	if err := json.Unmarshal(bb, &req2); err != nil {
		panic(err)
	}
	req2.Show()
}

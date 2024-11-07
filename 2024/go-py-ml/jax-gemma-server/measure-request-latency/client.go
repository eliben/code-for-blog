// This is a REST client that measures the average roundtrip latency of
// sending a request (and getting a response) to a prompt server in the parent
// directory, using its /echo endpoint.
package main

import (
	"flag"
	"fmt"
	"io"
	"net/http"
	"strings"
	"time"
)

var prompts = []string{
	"How do I make a classic French omelette?",
	"What are some tips for grilling the perfect steak?",
	"Can you suggest a vegan alternative to traditional cheese?",
	"How do I make homemade pasta from scratch?",
	"What are some unique ways to use basil in cooking?",
	"What is the best way to store fresh herbs?",
	"How can I make a creamy risotto without using dairy?",
	"What are some beginner-friendly recipes for baking bread?",
	"How can I make a quick and healthy breakfast smoothie?",
	"What spices pair well with roasted vegetables?",
	"What are the steps to make a traditional Japanese miso soup?",
	"How do I properly cook quinoa to avoid bitterness?",
	"What are some creative ways to use leftover rice?",
	"How can I make a gluten-free pizza crust?",
	"What’s the difference between broiling and baking in the oven?",
}

func main() {
	port := flag.Int("port", 20200, "Port where the prompt server listens")
	n := flag.Int("n", 10000, "Number of requests to send")
	flag.Parse()

	url := fmt.Sprintf("http://localhost:%d/echo", *port)

	t1 := time.Now()
	for i := range *n {
		msgBody := strings.NewReader(fmt.Sprintf("{\"prompt\": \"%s\"}", prompts[i%len(prompts)]))

		resp, err := http.Post(url, "application/json", msgBody)
		if err != nil {
			panic(err)
		}
		defer resp.Body.Close()

		body, err := io.ReadAll(resp.Body)
		if err != nil {
			panic(err)
		}
		if len(string(body)) < 1 {
			panic("bad response")
		}

	}

	elapsed := time.Since(t1)
	fmt.Printf("Num sends: %d;   Elapsed time: %s\n", *n, elapsed)
	fmt.Printf("Average time per request: %d ns\n", int(elapsed.Nanoseconds()) / *n)
}

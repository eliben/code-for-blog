// PubSub implementation taking a channel as argument in Subscibe, with Close.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"fmt"
	"slices"
	"sync"
	"time"
)

type Pubsub struct {
	mu     sync.RWMutex
	subs   map[string][]chan string
	closed bool
}

func NewPubsub() *Pubsub {
	ps := &Pubsub{}
	ps.subs = make(map[string][]chan string)
	ps.closed = false
	return ps
}

func (ps *Pubsub) Subscribe(topic string, ch chan string) {
	ps.mu.Lock()
	defer ps.mu.Unlock()

	ps.subs[topic] = append(ps.subs[topic], ch)
}

func (ps *Pubsub) Unsubscribe(topic string, ch chan string) {
	ps.mu.Lock()
	defer ps.mu.Unlock()

	ps.subs[topic] = slices.DeleteFunc(ps.subs[topic], func(c chan string) bool {
		return c == ch
	})
}

func (ps *Pubsub) Publish(topic string, msg string) {
	ps.mu.RLock()
	defer ps.mu.RUnlock()

	if ps.closed {
		return
	}

	for _, ch := range ps.subs[topic] {
		ch <- msg
	}
}

func (ps *Pubsub) Close() {
	ps.mu.Lock()
	defer ps.mu.Unlock()

	if !ps.closed {
		ps.closed = true
		for _, subs := range ps.subs {
			for _, ch := range subs {
				close(ch)
			}
		}
	}
}

func main() {
	ps := NewPubsub()
	makesub := func(topic string) chan string {
		ch := make(chan string, 1)
		ps.Subscribe(topic, ch)
		return ch
	}
	ch1 := makesub("tech")
	ch2 := makesub("travel")
	ch3 := makesub("travel")

	listener := func(name string, ch chan string) {
		for i := range ch {
			fmt.Printf("[%s] got %s\n", name, i)
		}
		fmt.Printf("[%s] done\n", name)
	}

	go listener("1", ch1)
	go listener("2", ch2)
	go listener("3", ch3)

	pub := func(topic string, msg string) {
		fmt.Printf("Publishing @%s: %s\n", topic, msg)
		ps.Publish(topic, msg)
		time.Sleep(1 * time.Millisecond)
	}

	time.Sleep(50 * time.Millisecond)
	pub("tech", "tablets")
	pub("health", "vitamins")
	pub("tech", "robots")
	pub("travel", "beaches")
	pub("travel", "hiking")
	pub("tech", "drones")

	time.Sleep(50 * time.Millisecond)

	ps.Unsubscribe("travel", ch2)
	pub("travel", "hiking")
	time.Sleep(50 * time.Millisecond)
	ps.Unsubscribe("travel", ch3)
	pub("travel", "hiking")
	time.Sleep(50 * time.Millisecond)

	ps.Close()
	time.Sleep(50 * time.Millisecond)
}

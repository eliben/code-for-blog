// The plugin binary implemeting the Couunter interface.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"sync"

	"example.com/shared"
	"github.com/hashicorp/go-plugin"
)

type MemoryCounter struct {
	sync.Mutex

	m map[string]int64
}

func NewMemoryCounter() *MemoryCounter {
	return &MemoryCounter{m: make(map[string]int64)}
}

func (mc *MemoryCounter) Put(key string, value int64, ah shared.AddHelper) {
	mc.Lock()
	defer mc.Unlock()

	sum, _ := ah.Sum(mc.m[key], value)
	mc.m[key] = sum
}

func (mc *MemoryCounter) Get(key string) int64 {
	mc.Lock()
	defer mc.Unlock()

	return mc.m[key]
}

func main() {
	counter := NewMemoryCounter()

	// pluginMap is the map of plugins we can dispense.
	pluginMap := map[string]plugin.Plugin{
		"counter": &shared.CounterPlugin{Impl: counter},
	}

	plugin.Serve(&plugin.ServeConfig{
		HandshakeConfig: shared.Handshake,
		Plugins:         pluginMap,
	})
}

// Main application (client) of the plugin.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"fmt"
	"log"
	"os/exec"

	"example.com/shared"
	"github.com/hashicorp/go-plugin"
)

func main() {
	pluginMap := map[string]plugin.Plugin{
		"counter": &shared.CounterPlugin{},
	}

	client := plugin.NewClient(&plugin.ClientConfig{
		HandshakeConfig: shared.Handshake,
		Plugins:         pluginMap,
		Cmd:             exec.Command("./plugin/counter"),
	})
	defer client.Kill()

	// Connect via RPC
	rpcClient, err := client.Client()
	if err != nil {
		log.Fatal(err)
	}

	// Request the plugin
	raw, err := rpcClient.Dispense("counter")
	if err != nil {
		log.Fatal(err)
	}

	// Obtain the shared.Counter interface implementation we can use to
	// interact with the plugin.
	counter := raw.(shared.Counter)

	ah := &addHelper{}
	counter.Put("joe", 20, ah)
	counter.Put("max", 15, ah)
	counter.Put("jill", 31, ah)
	counter.Put("joe", 8, ah)
	counter.Put("joe", 11, ah)
	counter.Put("jill", 6, ah)

	for _, name := range []string{"jill", "joe", "max"} {
		fmt.Printf("%v = %v\n", name, counter.Get(name))
	}
}

// addHelper implements the shared.AddHelper interface to perform addition,
// and is passed to the Put plugin calls.
type addHelper struct{}

func (*addHelper) Sum(x, y int64) (int64, error) {
	return x + y, nil
}

package main

import (
	"log"
	"os/exec"

	"example.com/plugin"
	goplugin "github.com/hashicorp/go-plugin"
)

func main() {
	pluginMap := map[string]goplugin.Plugin{
		"htmlize": &plugin.HtmlizePlugin{},
	}

	client := goplugin.NewClient(&goplugin.ClientConfig{
		HandshakeConfig: plugin.Handshake,
		Plugins:         pluginMap,
		Cmd:             exec.Command("./plugin-binaries/tt"),
	})
	defer client.Kill()

	rpcClient, err := client.Client()
	if err != nil {
		log.Fatal(err)
	}

	raw, err := rpcClient.Dispense("htmlize")
	if err != nil {
		log.Fatal(err)
	}

	htmlizer := raw.(plugin.Htmlizer)
	caps := htmlizer.Hooks()

	log.Println(caps)
}

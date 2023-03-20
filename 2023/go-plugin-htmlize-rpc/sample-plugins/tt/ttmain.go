package main

import (
	"example.com/plugin"
	goplugin "github.com/hashicorp/go-plugin"
)

type TtPluginInfo struct{}

func (TtPluginInfo) Hooks() []string {
	return []string{"role:tt"}
}

func main() {
	pluginMap := map[string]goplugin.Plugin{
		"htmlize": &plugin.HtmlizePlugin{
			InfoImpl: TtPluginInfo{},
		},
	}

	goplugin.Serve(&goplugin.ServeConfig{
		HandshakeConfig: plugin.Handshake,
		Plugins:         pluginMap,
	})
}

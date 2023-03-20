package main

import (
	"example.com/content"
	"example.com/plugin"
	goplugin "github.com/hashicorp/go-plugin"
)

type TtHtmlizer struct{}

func (TtHtmlizer) Hooks() []string {
	return []string{"role:tt"}
}

func (TtHtmlizer) ProcessContents(val string, post content.Post) string {
	return ""
}

func (TtHtmlizer) ProcessRole(role string, val string, post content.Post) string {
	return ""
}

func main() {
	pluginMap := map[string]goplugin.Plugin{
		"htmlize": &plugin.HtmlizePlugin{
			Impl: TtHtmlizer{},
		},
	}

	goplugin.Serve(&goplugin.ServeConfig{
		HandshakeConfig: plugin.Handshake,
		Plugins:         pluginMap,
	})
}

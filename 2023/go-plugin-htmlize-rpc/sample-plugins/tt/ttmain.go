package main

import (
	"fmt"

	"example.com/content"
	"example.com/plugin"
	goplugin "github.com/hashicorp/go-plugin"
)

type TtHtmlizer struct{}

func (TtHtmlizer) Hooks() []string {
	return []string{"role:tt"}
}

func (TtHtmlizer) ProcessContents(val string, post content.Post) string {
	return val
}

func (TtHtmlizer) ProcessRole(role string, val string, post content.Post) string {
	return fmt.Sprintf("<tt>%s</tt>", val)
}

func main() {
	goplugin.Serve(&goplugin.ServeConfig{
		HandshakeConfig: plugin.Handshake,
		Plugins: map[string]goplugin.Plugin{
			"htmlize": &plugin.HtmlizePlugin{
				Impl: TtHtmlizer{},
			},
		},
	})
}

package main

import (
	"fmt"
	"regexp"

	"example.com/content"
	"example.com/plugin"
	goplugin "github.com/hashicorp/go-plugin"
)

type NarcissistHtmlizer struct{}

func (NarcissistHtmlizer) Hooks() []string {
	return []string{"contents"}
}

var pattern = regexp.MustCompile(`\bI\b`)

func (NarcissistHtmlizer) ProcessContents(val string, post content.Post) string {
	repl := fmt.Sprintf("<b>I (%s)</b>", post.Author)
	return pattern.ReplaceAllString(val, repl)
}

func (NarcissistHtmlizer) ProcessRole(role string, val string, post content.Post) string {
	return val
}

func main() {
	goplugin.Serve(&goplugin.ServeConfig{
		HandshakeConfig: plugin.Handshake,
		Plugins: map[string]goplugin.Plugin{
			"htmlize": &plugin.HtmlizePlugin{
				Impl: NarcissistHtmlizer{},
			},
		},
	})
}

// Sample plugin that handles the whole post contents.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"fmt"
	"regexp"

	"example.com/content"
	"example.com/plugin"
)

var pattern = regexp.MustCompile(`\bI\b`)

func InitPlugin(pm *plugin.PluginManager) error {
	pm.RegisterContentsHook(narcissist)
	return nil
}

func narcissist(s string, db *content.DB, post *content.Post) string {
	repl := fmt.Sprintf("<b>I (%s)</b>", post.Author)
	return pattern.ReplaceAllString(s, repl)
}

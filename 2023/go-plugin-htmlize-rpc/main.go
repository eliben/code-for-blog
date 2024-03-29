// Main "htmlize" application that can load and register plugins from the
// plugin-binaries directory.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"fmt"
	"io"
	"log"
	"os"
	"regexp"
	"strings"

	"example.com/content"
	"example.com/plugin"
)

func main() {
	// Load plugins from the plugin-binaries directory.
	var pm plugin.Manager
	if err := pm.LoadPlugins("./plugin-binaries/"); err != nil {
		log.Fatal("loading plugins:", err)
	}
	defer pm.Close()

	// Read contents from stdin.
	contents, err := io.ReadAll(os.Stdin)
	if err != nil {
		log.Fatal(err)
	}

	// Create a dummy post to wrap the contents with additional metadata.
	post := &content.Post{
		Id:       42,
		Author:   "Joe",
		Contents: string(contents),
	}

	fmt.Printf("=== Text before htmlize:\n%s\n", post.Contents)
	result := htmlize(&pm, post)
	fmt.Printf("\n=== Text after htmlize:\n%s\n", result)
}

var rolePattern = regexp.MustCompile(":(\\w+):`([^`]*)`")

// htmlize turns the text of post.Contents into HTML and returns it; it uses
// the plugin manager to invoke loaded plugins on the contents and the roles
// within it.
func htmlize(pm *plugin.Manager, post *content.Post) string {
	pcontents := insertParagraphs(post.Contents)
	pcontents = pm.ApplyContentsHooks(pcontents, post)

	return rolePattern.ReplaceAllStringFunc(pcontents, func(c string) string {
		subm := rolePattern.FindStringSubmatch(c)
		if len(subm) < 3 {
			panic("expect match")
		}
		roleName := subm[1]
		roleText := subm[2]

		repl, err := pm.ApplyRoleHooks(roleName, roleText, post)
		if err == nil {
			return repl
		} else {
			return c
		}
	})
}

func insertParagraphs(s string) string {
	var b strings.Builder
	p := 0

	for p < len(s) {
		// Loop invariant: p is the index of the beginning of the current paragraph.
		var paragraph string
		nextBreak := strings.Index(s[p:], "\n\n")
		if nextBreak >= 0 {
			nextBreak = p + nextBreak
			paragraph = s[p:nextBreak]
			p = nextBreak + 1
		} else {
			paragraph = strings.TrimSpace(s[p:])
			p = len(s)
		}

		paragraph = strings.Join(strings.Split(paragraph, "\n"), " ")
		b.WriteString("<p>")
		b.WriteString(paragraph)
		b.WriteString("</p>\n\n")

		// Re-point p to the start of the next parapraph.
		for p < len(s) && s[p] == '\n' {
			p++
		}
	}
	return b.String()
}

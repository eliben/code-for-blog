// Main entry point for the plugin sample program.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"flag"
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
	pluginDir := flag.String("plugindir", ".", "directory with plugins to load")
	flag.Parse()

	pm, err := plugin.LoadPlugins(*pluginDir)
	if err != nil {
		log.Fatal(err)
	}

	contents, err := io.ReadAll(os.Stdin)
	if err != nil {
		log.Fatal(err)
	}

	post := &content.Post{Id: 42, Author: "Joe", Contents: string(contents)}
	db := &content.DB{Name: "maindb"}

	fmt.Printf("=== Text before htmlize:\n%s\n", post.Contents)
	result := htmlize(pm, db, post)

	fmt.Printf("\n=== Text afterhtmlize:\n%s\n", result)
}

var rolePattern = regexp.MustCompile(":(\\w+):`([^`]*)`")

// htmlize turns the text of post.Contents into HTML and returns it; it uses
// the plugin manager to invoke registered plugins on the contents and the roles
// within it.
func htmlize(pm *plugin.PluginManager, db *content.DB, post *content.Post) string {
	pcontents := insertParagraphs(post.Contents)
	pcontents = pm.ApplyContentsHooks(pcontents, db, post)

	return rolePattern.ReplaceAllStringFunc(pcontents, func(c string) string {
		subm := rolePattern.FindStringSubmatch(c)
		if len(subm) < 3 {
			panic("expect match")
		}
		roleName := subm[1]
		roleText := subm[2]

		repl, err := pm.ApplyRoleHooks(roleName, roleText, db, post)
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

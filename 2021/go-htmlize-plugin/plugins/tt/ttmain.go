// Sample plugin that handles `tt` roles.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"fmt"

	"example.com/content"
	"example.com/plugin"
)

func InitPlugin(pm *plugin.PluginManager) error {
	pm.RegisterRoleHook("tt", tt)
	return nil
}

func tt(s string, db *content.DB, post *content.Post) string {
	return fmt.Sprintf("<tt>%s</tt>", s)
}

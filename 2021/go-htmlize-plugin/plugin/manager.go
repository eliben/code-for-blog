// Package plugin serves as the bridge between the main application and plugins.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package plugin

import (
	"fmt"

	"example.com/content"
)

// RoleHook takes the role contents, DB and Post and returns the text this role
// should be replaced with.
type RoleHook func(string, *content.DB, *content.Post) string

// ContentsHook takes the post contents, DB and Post and returns the replacement
// contents.
type ContentsHook func(string, *content.DB, *content.Post) string

type PluginManager struct {
	roleHooks     map[string]RoleHook
	contentsHooks []ContentsHook
}

func newPluginManager() *PluginManager {
	pm := &PluginManager{}
	pm.roleHooks = make(map[string]RoleHook)
	return pm
}

func (pm *PluginManager) RegisterRoleHook(rolename string, hook RoleHook) {
	pm.roleHooks[rolename] = hook
}

func (pm *PluginManager) RegisterContentsHook(hook ContentsHook) {
	pm.contentsHooks = append(pm.contentsHooks, hook)
}

func (pm *PluginManager) ApplyRoleHooks(rolename string, roletext string, db *content.DB, post *content.Post) (string, error) {
	if hook, ok := pm.roleHooks[rolename]; ok {
		return hook(roletext, db, post), nil
	} else {
		return "", fmt.Errorf("no hook for role '%s' found", rolename)
	}
}

func (pm *PluginManager) ApplyContentsHooks(contents string, db *content.DB, post *content.Post) string {
	for _, hook := range pm.contentsHooks {
		contents = hook(contents, db, post)
	}
	return contents
}

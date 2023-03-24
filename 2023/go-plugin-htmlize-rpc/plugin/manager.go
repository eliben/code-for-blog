// Plugin manager used by the main application to load and invoke plugins.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package plugin

import (
	"fmt"
	"os/exec"
	"strings"

	"example.com/content"
	goplugin "github.com/hashicorp/go-plugin"
)

// Manager loads and manages Htmlizer plugins for this application.
//
// After creating a Manager value, call LoadPlugins with a directory path to
// discover and load plugins. At the end of the program call Close to kill and
// clean up all plugin processes.
type Manager struct {
	roleHooks     map[string]Htmlizer
	contentsHooks []Htmlizer

	pluginClients []*goplugin.Client
}

// LoadPlugins takes a directory path and assumes that all files within it
// are plugin binaries. It runs all these binaries in sub-processes,
// establishes RPC communication with the plugins, and registers them for
// the hooks they declare to support.
func (m *Manager) LoadPlugins(path string) error {
	m.contentsHooks = []Htmlizer{}
	m.roleHooks = make(map[string]Htmlizer)

	binaries, err := goplugin.Discover("*", path)
	if err != nil {
		return err
	}

	pluginMap := map[string]goplugin.Plugin{
		"htmlize": &HtmlizePlugin{},
	}

	for _, bpath := range binaries {
		client := goplugin.NewClient(&goplugin.ClientConfig{
			HandshakeConfig: Handshake,
			Plugins:         pluginMap,
			Cmd:             exec.Command(bpath),
		})
		m.pluginClients = append(m.pluginClients, client)

		rpcClient, err := client.Client()
		if err != nil {
			return err
		}

		raw, err := rpcClient.Dispense("htmlize")
		if err != nil {
			return err
		}

		impl := raw.(Htmlizer)

		// Query the plugin for its capabilities -- the hooks it supports.
		// Based on this information, register the plugin with the appropriate
		// role or contents hooks.
		capabilities := impl.Hooks()

		for _, cap := range capabilities {
			if cap == "contents" {
				m.contentsHooks = append(m.contentsHooks, impl)
			} else {
				parts := strings.Split(cap, ":")
				if len(parts) == 2 && parts[0] == "role" {
					m.roleHooks[parts[1]] = impl
				}
			}
		}
	}

	return nil
}

func (m *Manager) Close() {
	for _, client := range m.pluginClients {
		client.Kill()
	}
}

// ApplyRoleHooks applies a registered plugin to the given role: name and text,
// returning the transformed value. Only the last registered plugin is
// applied.
func (m *Manager) ApplyRoleHooks(rolename, roletext string, post *content.Post) (string, error) {
	if hook, ok := m.roleHooks[rolename]; ok {
		return hook.ProcessRole(rolename, roletext, *post), nil
	} else {
		return "", fmt.Errorf("no hook for role '%s' found", rolename)
	}
}

// ApplyContentsHooks applies registered plugins to the given post contents,
// returning the transformed value. All registered plugins are applied in
// sequence to the value.
func (m *Manager) ApplyContentsHooks(contents string, post *content.Post) string {
	for _, hook := range m.contentsHooks {
		contents = hook.ProcessContents(contents, *post)
	}
	return contents
}

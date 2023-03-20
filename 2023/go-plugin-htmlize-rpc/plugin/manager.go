package plugin

import (
	"fmt"
	"log"
	"os/exec"
	"strings"

	"example.com/content"
	goplugin "github.com/hashicorp/go-plugin"
)

type Manager struct {
	roleHooks     map[string]Htmlizer
	contentsHooks []Htmlizer

	pluginClients []*goplugin.Client
}

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
			log.Fatal(err)
		}

		raw, err := rpcClient.Dispense("htmlize")
		if err != nil {
			log.Fatal(err)
		}

		impl := raw.(Htmlizer)
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

func (m *Manager) ApplyRoleHooks(rolename, roletext string, post *content.Post) (string, error) {
	if hook, ok := m.roleHooks[rolename]; ok {
		return hook.ProcessRole(rolename, roletext, *post), nil
	} else {
		return "", fmt.Errorf("no hook for role '%s' found", rolename)
	}
}

func (m *Manager) ApplyContentsHooks(contents string, post *content.Post) string {
	for _, hook := range m.contentsHooks {
		contents = hook.ProcessContents(contents, *post)
	}
	return contents
}

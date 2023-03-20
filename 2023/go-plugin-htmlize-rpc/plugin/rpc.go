package plugin

import (
	"log"
	"net/rpc"

	"github.com/hashicorp/go-plugin"
)

type PluginServerRPC struct {
	InfoImpl     PluginInfo
	RoleImpl     RoleHook
	ContentsImpl ContentsHook
}

type Empty struct{}

type HooksReply struct {
	Hooks []string
}

func (s *PluginServerRPC) Hooks(Empty, reply *HooksReply) error {
	reply.Hooks = s.InfoImpl.Hooks()
	return nil
}

type PluginClientRPC struct {
	client *rpc.Client
}

func (c *PluginClientRPC) Hooks() []string {
	var reply HooksReply
	if err := c.client.Call("Plugin.Hooks", Empty{}, &reply); err != nil {
		log.Fatal(err)
	}
	return reply.Hooks
}

type HtmlizePlugin struct {
	InfoImpl     PluginInfo
	RoleImpl     RoleHook
	ContentsImpl ContentsHook
}

func (p *HtmlizePlugin) Server(*plugin.MuxBroker) (interface{}, error) {
	return &PluginServerRPC{
		InfoImpl:     p.InfoImpl,
		RoleImpl:     p.RoleImpl,
		ContentsImpl: p.ContentsImpl,
	}, nil
}

func (p *HtmlizePlugin) Client(b *plugin.MuxBroker, c *rpc.Client) (interface{}, error) {
	return &PluginClientRPC{
		client: c,
	}, nil
}

package plugin

import (
	"log"
	"net/rpc"

	"example.com/content"
	"github.com/hashicorp/go-plugin"
)

type PluginServerRPC struct {
	Impl Htmlizer
}

type Empty struct{}

type HooksReply struct {
	Hooks []string
}

func (s *PluginServerRPC) Hooks(Empty, reply *HooksReply) error {
	reply.Hooks = s.Impl.Hooks()
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

type ContentsArgs struct {
	Value string
	Post  content.Post
}

type ContentsReply struct {
	Value string
}

func (c *PluginClientRPC) ProcessContents(val string, post content.Post) string {
	var reply ContentsReply
	if err := c.client.Call(
		"Plugin.ProcessContents",
		ContentsArgs{Value: val, Post: post},
		&reply); err != nil {
		log.Fatal(err)
	}
	return reply.Value
}

type RoleArgs struct {
	Role  string
	Value string
	Post  content.Post
}

type RoleReply struct {
	Value string
}

func (c *PluginClientRPC) ProcessRole(role string, val string, post content.Post) string {
	var reply RoleReply
	if err := c.client.Call(
		"Plugin.ProcessRole",
		RoleArgs{Role: role, Value: val, Post: post},
		&reply); err != nil {
		log.Fatal(err)
	}
	return reply.Value
}

type HtmlizePlugin struct {
	Impl Htmlizer
}

func (p *HtmlizePlugin) Server(*plugin.MuxBroker) (interface{}, error) {
	return &PluginServerRPC{
		Impl: p.Impl,
	}, nil
}

func (p *HtmlizePlugin) Client(b *plugin.MuxBroker, c *rpc.Client) (interface{}, error) {
	return &PluginClientRPC{
		client: c,
	}, nil
}

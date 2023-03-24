// RPC scaffolding for our server and client, using net/rpc.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package plugin

import (
	"log"
	"net/rpc"

	"example.com/content"
)

// Types for RPC args/reply messages.

type HooksArgs struct{}

type HooksReply struct {
	Hooks []string
}

type ContentsArgs struct {
	Value string
	Post  content.Post
}

type ContentsReply struct {
	Value string
}

type RoleArgs struct {
	Role  string
	Value string
	Post  content.Post
}

type RoleReply struct {
	Value string
}

// PluginServerRPC is used by plugins to map RPC calls from the clients to
// methods of the Htmlizer interface.
type PluginServerRPC struct {
	Impl Htmlizer
}

func (s *PluginServerRPC) Hooks(args HooksArgs, reply *HooksReply) error {
	reply.Hooks = s.Impl.Hooks()
	return nil
}

func (s *PluginServerRPC) ProcessContents(args ContentsArgs, reply *ContentsReply) error {
	reply.Value = s.Impl.ProcessContents(args.Value, args.Post)
	return nil
}

func (s *PluginServerRPC) ProcessRole(args RoleArgs, reply *RoleReply) error {
	reply.Value = s.Impl.ProcessRole(args.Role, args.Value, args.Post)
	return nil
}

// PluginClientRPC is used by clients (main application) to translate the
// Htmlize interface of plugins to RPC calls.
type PluginClientRPC struct {
	client *rpc.Client
}

func (c *PluginClientRPC) Hooks() []string {
	var reply HooksReply
	if err := c.client.Call("Plugin.Hooks", HooksArgs{}, &reply); err != nil {
		log.Fatal(err)
	}
	return reply.Hooks
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

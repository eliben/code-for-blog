package plugin

import (
	"net/rpc"

	"github.com/hashicorp/go-plugin"
)

// HtmlizePlugin implements the plugin.Plugin interface to provide the RPC
// server or client back to the plugin machinery. The server side should
// proved the Impl field with a concrete implementation of the Htmlizer
// interface.
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

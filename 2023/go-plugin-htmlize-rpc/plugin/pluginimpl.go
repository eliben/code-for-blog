package plugin

import (
	"net/rpc"

	"github.com/hashicorp/go-plugin"
)

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

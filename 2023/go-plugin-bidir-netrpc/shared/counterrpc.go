// RPC scaffolding for the Counter interface.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package shared

import (
	"net/rpc"

	"github.com/hashicorp/go-plugin"
)

type CounterPlugin struct {
	Impl Counter
}

func (p *CounterPlugin) Server(broker *plugin.MuxBroker) (interface{}, error) {
	return &CounterServerRPC{
		Impl:   p.Impl,
		broker: broker,
	}, nil
}

func (CounterPlugin) Client(broker *plugin.MuxBroker, c *rpc.Client) (interface{}, error) {
	return &CounterClientRPC{
		client: c,
		broker: broker,
	}, nil
}

type CounterClientRPC struct {
	client *rpc.Client
	broker *plugin.MuxBroker
}

type PutArgs struct {
	AddServer uint32
	Key       string
	Value     int64
}

type GetArgs struct {
	Key string
}

type GetReply struct {
	Value int64
}

func (c *CounterClientRPC) Put(key string, value int64, ah AddHelper) {
	// The client calls this Put method to perform a "Put" operation in the
	// plugin. Since this requires the plugin to call back into the client for
	// the AddHelper implementation, we have to set up an RPC server for the
	// duration of this call.
	//
	// This is done by the broker that was passed to us from go-plugin; the
	// broker lets us create a multiplexed server listening on the same
	// connection. We obtain the channel ID from the broker and pass it to the
	// plugin so it knows where to connect.
	addServerID := c.broker.NextId()
	addServer := &AddHelperServerRPC{Impl: ah}

	// Broker's AcceptAndServe serves a single connection. Once the client hangs
	// up, this cleans up.
	go c.broker.AcceptAndServe(addServerID, addServer)

	if err := c.client.Call("Plugin.Put", PutArgs{
		AddServer: addServerID,
		Key:       key,
		Value:     value}, nil); err != nil {
		panic(err)
	}
}

func (c *CounterClientRPC) Get(key string) int64 {
	var reply GetReply
	if err := c.client.Call("Plugin.Get", GetArgs{Key: key}, &reply); err != nil {
		panic(err)
	}

	return reply.Value
}

type CounterServerRPC struct {
	Impl   Counter
	broker *plugin.MuxBroker
}

type EmptyReply struct{}

func (s *CounterServerRPC) Put(args PutArgs, resp *EmptyReply) error {
	conn, err := s.broker.Dial(args.AddServer)
	if err != nil {
		return err
	}
	defer conn.Close()

	addClient := rpc.NewClient(conn)
	s.Impl.Put(args.Key, args.Value, &AddHelperClientRPC{client: addClient})
	return nil
}

func (s *CounterServerRPC) Get(args GetArgs, resp *GetReply) error {
	v := s.Impl.Get(args.Key)
	resp.Value = v
	return nil
}

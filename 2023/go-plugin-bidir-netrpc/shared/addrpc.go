// RPC scaffolding for the AddHelper interface.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package shared

import "net/rpc"

type AddHelperServerRPC struct {
	Impl AddHelper
}

type SumArgs struct {
	X, Y int64
}

type SumReply struct {
	Result int64
}

func (a *AddHelperServerRPC) Sum(args SumArgs, resp *SumReply) error {
	r, err := a.Impl.Sum(args.X, args.Y)
	if err != nil {
		return err
	}
	resp.Result = r
	return nil
}

// AddHelperClientRPC is the RPC client used by the plugin.
//
// It can be a bit confusing because plugin is typically the server, but it's
// OK for a server to call other RPC servers; to do that, it needs to create an
// RPC client.
type AddHelperClientRPC struct {
	client *rpc.Client
}

func (a *AddHelperClientRPC) Sum(x int64, y int64) (int64, error) {
	// To perform this operation we call back to the main application.
	var resp SumReply
	if err := a.client.Call("Plugin.Sum", SumArgs{X: x, Y: y}, &resp); err != nil {
		return 0, err
	}
	return resp.Result, nil
}

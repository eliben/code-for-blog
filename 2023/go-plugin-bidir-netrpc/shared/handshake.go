// Common handshake between plugin and main application.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package shared

import "github.com/hashicorp/go-plugin"

// Handshake is a common handshake that is shared by plugin and host.
var Handshake = plugin.HandshakeConfig{
	ProtocolVersion:  1,
	MagicCookieKey:   "BASIC_PLUGIN",
	MagicCookieValue: "hello",
}

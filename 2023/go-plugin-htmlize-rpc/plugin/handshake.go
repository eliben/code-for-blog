package plugin

import goplugin "github.com/hashicorp/go-plugin"

// Handshake is a common handshake that is shared by plugin and host.
var Handshake = goplugin.HandshakeConfig{
	ProtocolVersion:  1,
	MagicCookieKey:   "HTMLIZE_PLUGIN",
	MagicCookieValue: "hello",
}

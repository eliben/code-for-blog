// The interface implemented by the plugin.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package shared

// Counter is the interface implemented by plugins.
type Counter interface {
	Put(key string, value int64, ah AddHelper)
	Get(key string) int64
}

// AddHelper is the interface implemented by the main application and passed
// into the plugin when needed, for bidirectional calls.
type AddHelper interface {
	Sum(int64, int64) (int64, error)
}

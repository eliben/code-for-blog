// Data model for the task server.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package swagger

import (
	"time"
)

type Body struct {
	Text string    `json:"text,omitempty"`
	Tags []string  `json:"tags,omitempty"`
	Due  time.Time `json:"due,omitempty"`
}

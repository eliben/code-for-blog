// Resolver type.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package graph

import (
	"example.com/internal/taskstore"
)

type Resolver struct {
	Store *taskstore.TaskStore
}

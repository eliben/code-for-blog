package graph

import (
	"example.com/internal/taskstore"
)

type Resolver struct {
	Store *taskstore.TaskStore
}

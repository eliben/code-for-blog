package main

import (
	"net/http"
	"os"
	"sync"

	"example.com/internal/taskstore"
	"github.com/gin-gonic/gin"
)

type taskServer struct {
	// Mutex protects access to the 'store' field. We don't make assumptions
	// about the safety of any method of store, so all accesses are protected.
	sync.Mutex
	store *taskstore.TaskStore
}

func NewTaskServer() *taskServer {
	store := taskstore.New()
	return &taskServer{store: store}
}

func (ts *taskServer) getAllTasksHandler(c *gin.Context) {
	ts.Lock()
	allTasks := ts.store.GetAllTasks()
	ts.Unlock()

	c.JSON(http.StatusOK, allTasks)
}

func main() {
	engine := gin.Default()
	server := NewTaskServer()

	engine.GET("/task/", server.getAllTasksHandler)

	// TODO: need StrictSlash equivalent?
	// TODO: note that Default() already has some default middleware setup

	engine.Run("localhost:" + os.Getenv("SERVERPORT"))
}

package main

import (
	"net/http"
	"os"
	"strconv"
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

func (ts *taskServer) getTaskHandler(c *gin.Context) {
	id, err := strconv.Atoi(c.Params.ByName("id"))
	if err != nil {
		c.String(http.StatusBadRequest, err.Error())
		return
	}

	ts.Lock()
	task, err := ts.store.GetTask(id)
	ts.Unlock()

	if err != nil {
		c.String(http.StatusNotFound, err.Error())
		return
	}

	c.JSON(http.StatusOK, task)
}

func main() {
	router := gin.Default()
	server := NewTaskServer()

	router.GET("/task/", server.getAllTasksHandler)
	router.GET("/task/:id", server.getTaskHandler)

	// TODO: need StrictSlash equivalent?
	// TODO: note that Default() already has some default middleware setup

	router.Run("localhost:" + os.Getenv("SERVERPORT"))
}

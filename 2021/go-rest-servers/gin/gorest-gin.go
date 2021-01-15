package main

import (
	"net/http"
	"os"
	"strconv"
	"sync"
	"time"

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

func (ts *taskServer) deleteAllTasksHandler(c *gin.Context) {
	ts.Lock()
	ts.store.DeleteAllTasks()
	ts.Unlock()
}

func (ts *taskServer) createTaskHandler(c *gin.Context) {
	type RequestTask struct {
		Text string    `json:"text"`
		Tags []string  `json:"tags"`
		Due  time.Time `json:"due"`
	}

	var rt RequestTask
	if err := c.ShouldBindJSON(&rt); err != nil {
		c.String(http.StatusBadRequest, err.Error())
	}

	ts.Lock()
	id := ts.store.CreateTask(rt.Text, rt.Tags, rt.Due)
	ts.Unlock()

	c.JSON(http.StatusOK, gin.H{"Id": id})
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

func (ts *taskServer) deleteTaskHandler(c *gin.Context) {
	id, err := strconv.Atoi(c.Params.ByName("id"))
	if err != nil {
		c.String(http.StatusBadRequest, err.Error())
		return
	}

	ts.Lock()
	err = ts.store.DeleteTask(id)
	ts.Unlock()

	if err != nil {
		c.String(http.StatusNotFound, err.Error())
	}
}

func (ts *taskServer) tagHandler(c *gin.Context) {
	tag := c.Params.ByName("tag")
	ts.Lock()
	tasks := ts.store.GetTasksByTag(tag)
	ts.Unlock()

	c.JSON(http.StatusOK, tasks)
}

func (ts *taskServer) dueHandler(c *gin.Context) {
	badRequestError := func() {
		c.String(http.StatusBadRequest, "expect /due/<year>/<month>/<day>, got %v", c.FullPath())
	}

	year, err := strconv.Atoi(c.Params.ByName("year"))
	if err != nil {
		badRequestError()
		return
	}

	month, err := strconv.Atoi(c.Params.ByName("month"))
	if err != nil || month < int(time.January) || month > int(time.December) {
		badRequestError()
		return
	}

	day, err := strconv.Atoi(c.Params.ByName("day"))
	if err != nil {
		badRequestError()
		return
	}

	ts.Lock()
	tasks := ts.store.GetTasksByDueDate(year, time.Month(month), day)
	ts.Unlock()

	c.JSON(http.StatusOK, tasks)
}

func main() {
	router := gin.Default()
	server := NewTaskServer()

	router.POST("/task/", server.createTaskHandler)
	router.GET("/task/", server.getAllTasksHandler)
	router.DELETE("/task/", server.deleteAllTasksHandler)
	router.GET("/task/:id", server.getTaskHandler)
	router.DELETE("/task/:id", server.deleteTaskHandler)
	router.GET("/tag/:tag", server.tagHandler)
	router.GET("/due/:year/:month/:day", server.dueHandler)

	// TODO: need StrictSlash equivalent?
	// TODO: note that Default() already has some default middleware setup

	router.Run("localhost:" + os.Getenv("SERVERPORT"))
}

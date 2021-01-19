package main

import (
	"net/http"
	"os"
	"strconv"
	"time"

	"example.com/internal/taskstore"
	"github.com/gin-gonic/gin"
)

type taskServer struct {
	store *taskstore.TaskStore
}

func NewTaskServer() *taskServer {
	store := taskstore.New()
	return &taskServer{store: store}
}

func (ts *taskServer) getAllTasksHandler(c *gin.Context) {
	allTasks := ts.store.GetAllTasks()
	c.JSON(http.StatusOK, allTasks)
}

func (ts *taskServer) deleteAllTasksHandler(c *gin.Context) {
	ts.store.DeleteAllTasks()
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

	id := ts.store.CreateTask(rt.Text, rt.Tags, rt.Due)
	c.JSON(http.StatusOK, gin.H{"Id": id})
}

func (ts *taskServer) getTaskHandler(c *gin.Context) {
	id, err := strconv.Atoi(c.Params.ByName("id"))
	if err != nil {
		c.String(http.StatusBadRequest, err.Error())
		return
	}

	task, err := ts.store.GetTask(id)
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

	if err = ts.store.DeleteTask(id); err != nil {
		c.String(http.StatusNotFound, err.Error())
	}
}

func (ts *taskServer) tagHandler(c *gin.Context) {
	tag := c.Params.ByName("tag")
	tasks := ts.store.GetTasksByTag(tag)
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

	tasks := ts.store.GetTasksByDueDate(year, time.Month(month), day)
	c.JSON(http.StatusOK, tasks)
}

func main() {
	router := gin.Default()
	server := NewTaskServer()

	// TODO: non standard signature for handlers, but check out WrapF/WrapH

	// TODO: no regex in matcher: example of limitation

	router.POST("/task/", server.createTaskHandler)
	router.GET("/task/", server.getAllTasksHandler)
	router.DELETE("/task/", server.deleteAllTasksHandler)
	router.GET("/task/:id", server.getTaskHandler)
	router.DELETE("/task/:id", server.deleteTaskHandler)
	router.GET("/tag/:tag", server.tagHandler)
	router.GET("/due/:year/:month/:day", server.dueHandler)

	// TODO: note that Default() already has some default middleware setup
	// logger and crash recovery (check them out). It's easy to see the logger
	// in action, and should be easy to check for crashes (try to panic here and
	// in the no-framework handler?)
	// Note that stdlib also has a panic interceptor that aborts the request but
	// not the whole server.

	router.Run("localhost:" + os.Getenv("SERVERPORT"))
}

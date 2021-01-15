// Testing for taskstore servers; running tests assume a server runs at a fixed
// port specified by the SERVERPORT env var.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package server

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"testing"
	"time"
)

// Note: this struct should be kept in sync with the actual Task in servers.
// It's copied here to avoid any dependencies for this module.
type Task struct {
	Id   int       `json:"id"`
	Text string    `json:"text"`
	Tags []string  `json:"tags"`
	Due  time.Time `json:"due"`
}

func serverAddress() string {
	port := os.Getenv("SERVERPORT")
	return "http://localhost:" + port
}

// resetServer "resets" the server by asking it to delete all previously created
// tasks. This helps run tests in isolation from each other.
func resetServer(t *testing.T) {
	req, err := http.NewRequest("DELETE", serverAddress()+"/task/", nil)
	if err != nil {
		t.Fatal(err)
	}

	var client http.Client
	resp, err := client.Do(req)
	if err != nil {
		log.Fatal(err)
	}
	resp.Body.Close()
}

// createTask sends a request to the server asking to create a task with the
// given attributes. It verifies the server response and returns the ID for the
// new task.
func createTask(t *testing.T, text string, tags []string, due time.Time) int {
	taskValue := CreateTaskRequest{
		Text: text,
		Tags: tags,
		Due:  due,
	}

	taskStr, err := json.Marshal(taskValue)
	if err != nil {
		t.Fatal(err)
	}
	reqBody := bytes.NewBuffer(taskStr)

	resp, err := http.Post(serverAddress()+"/task/", "application/json", reqBody)
	if err != nil {
		t.Fatal(err)
	}
	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		t.Fatal(err)
	}

	var respValue CreateTaskResponse
	if err := json.Unmarshal(body, &respValue); err != nil {
		t.Fatal(err)
	}

	return respValue.Id
}

func enforceContentJSON(t *testing.T, resp *http.Response) {
	t.Helper()
	contentType, ok := resp.Header["Content-Type"]
	if !ok {
		t.Fatalf("want response header to have Content-Type, got %v", resp.Header)
	}
	if len(contentType) < 1 || !(contentType[0] == "application/json" || contentType[0] == "application/json; charset=utf-8") {
		t.Errorf("want Content-Type=application/json, got %v", contentType)
	}
}

// getAllTasks obtains a list of all tasks in the server's store.
func getAllTasks(t *testing.T) []Task {
	t.Helper()
	resp, err := http.Get(serverAddress() + "/task/")
	if err != nil {
		t.Fatal(err)
	}
	defer resp.Body.Close()

	enforceContentJSON(t, resp)
	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		t.Fatal(err)
	}

	var tasks []Task
	if err := json.Unmarshal(body, &tasks); err != nil {
		log.Fatal(err)
	}
	return tasks
}

// getTasksByDueDate gets all tasks from the server by due date.
func getTasksByDueDate(t *testing.T, year int, month time.Month, day int) []Task {
	t.Helper()

	resp, err := http.Get(serverAddress() + fmt.Sprintf("/due/%d/%d/%d", year, int(month), day))
	if err != nil {
		t.Fatal(err)
	}
	defer resp.Body.Close()

	enforceContentJSON(t, resp)
	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		t.Fatal(err)
	}

	var tasks []Task
	if err := json.Unmarshal(body, &tasks); err != nil {
		log.Fatal(err)
	}
	return tasks
}

// getTasksByTag gets all tasks from the server with the given tag.
func getTasksByTag(t *testing.T, tag string) []Task {
	t.Helper()

	resp, err := http.Get(serverAddress() + fmt.Sprintf("/tag/%s/", tag))
	if err != nil {
		t.Fatal(err)
	}
	defer resp.Body.Close()

	enforceContentJSON(t, resp)
	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		t.Fatal(err)
	}

	var tasks []Task
	if err := json.Unmarshal(body, &tasks); err != nil {
		log.Fatal(err)
	}
	return tasks
}

// getTaskById expects to find the task identified by id on the server, and
// returns it. If the task is not found or some other error occurs, fails the
// test.
func getTaskById(t *testing.T, id int) Task {
	t.Helper()

	resp, err := http.Get(serverAddress() + fmt.Sprintf("/task/%d/", id))
	if err != nil {
		t.Fatal(err)
	}
	defer resp.Body.Close()

	enforceContentJSON(t, resp)
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("want status code=OK, got %v", resp.StatusCode)
	}

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		t.Fatal(err)
	}

	var taskValue Task
	if err := json.Unmarshal(body, &taskValue); err != nil {
		t.Fatal(err)
	}

	return taskValue
}

// expectTaskNotFound expects the server to not find a task identified by id.
func expectTaskNotFound(t *testing.T, id int) {
	t.Helper()

	resp, err := http.Get(serverAddress() + fmt.Sprintf("/task/%d/", id))
	if err != nil {
		t.Fatal(err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusNotFound {
		t.Fatalf("want status code=StatusNotFound, got %v", resp.StatusCode)
	}
}

func deleteTaskById(t *testing.T, id int) {
	t.Helper()

	req, err := http.NewRequest("DELETE", serverAddress()+fmt.Sprintf("/task/%d/", id), nil)
	if err != nil {
		t.Fatal(err)
	}

	var client http.Client
	resp, err := client.Do(req)
	if err != nil {
		log.Fatal(err)
	}
	resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		t.Fatalf("want status code=OK, got %v", resp.StatusCode)
	}
}

func TestAlive(t *testing.T) {
	// Basic "it's alive" test. Send a request to get all tasks, expect a valid
	// response.
	getAllTasks(t)
}

type CreateTaskRequest struct {
	Text string    `json:"text"`
	Tags []string  `json:"tags"`
	Due  time.Time `json:"due"`
}

type CreateTaskResponse struct {
	Id int `json:"id"`
}

func TestCreateAndGet(t *testing.T) {
	// Creates a task, then gets it back from the server.
	resetServer(t)

	id1 := createTask(t, "zbor", nil, time.Now())

	task := getTaskById(t, id1)
	if task.Text != "zbor" {
		t.Errorf("want found task text %q, got %q", "zbor", task.Text)
	}

	expectTaskNotFound(t, id1+100)

	// Now create another task, and expect both to be found.
	id2 := createTask(t, "xyz", nil, time.Now())

	task1 := getTaskById(t, id1)
	if task1.Text != "zbor" {
		t.Errorf("want found task text %q, got %q", "zbor", task1.Text)
	}

	task2 := getTaskById(t, id2)
	if task2.Text != "xyz" {
		t.Errorf("want found task text %q, got %q", "xyz", task2.Text)
	}
}

func TestCreateAndDelete(t *testing.T) {
	// Creates some tasks, then deletes some.
	resetServer(t)

	id1 := createTask(t, "aaa", nil, time.Now())
	id2 := createTask(t, "bbb", nil, time.Now())

	getTaskById(t, id1)
	getTaskById(t, id2)

	deleteTaskById(t, id2)
	getTaskById(t, id1)
	expectTaskNotFound(t, id2)
}

func TestCreateAndList(t *testing.T) {
	// Creates some tasks, then lists them.
	resetServer(t)

	id1 := createTask(t, "xyz", nil, time.Now())
	id2 := createTask(t, "abc", nil, time.Now())
	id3 := createTask(t, "tuv", nil, time.Now())

	tasks := getAllTasks(t)

	checkTaskHasText := func(id int, text string) {
		t.Helper()
		for _, task := range tasks {
			if task.Id == id {
				if text != task.Text {
					t.Errorf("want text=%v for task id=%v, got %v", text, id, task.Text)
				}
				return
			}
		}
		// If we're here, task wasn't found.
		t.Errorf("task id=%v not found", id)
	}

	checkTaskHasText(id1, "xyz")
	checkTaskHasText(id2, "abc")
	checkTaskHasText(id3, "tuv")
}

func TestGetByTag(t *testing.T) {
	// Get tasks by tag.
	resetServer(t)

	createTask(t, "xyz", []string{"movies", "go"}, time.Now())
	createTask(t, "abc", []string{"go", "books"}, time.Now())
	createTask(t, "tuv", []string{"books"}, time.Now())

	var tests = []struct {
		tag string
		n   int
	}{
		{"maximus", 0},
		{"books", 2},
		{"go", 2},
		{"movies", 1},
		{"foo", 0},
	}

	for _, tt := range tests {
		t.Run(tt.tag, func(t *testing.T) {
			tasks := getTasksByTag(t, tt.tag)
			if len(tasks) != tt.n {
				t.Errorf("want %d books tasks, got %v", tt.n, tasks)
			}
		})
	}
}

func TestGetByDueDate(t *testing.T) {
	// Get tasks by due date.
	resetServer(t)

	createTask(t, "xyz", nil, time.Date(2010, time.November, 7, 0, 0, 0, 0, time.UTC))
	createTask(t, "x22", nil, time.Date(2012, time.January, 19, 0, 0, 0, 0, time.UTC))
	createTask(t, "x33", nil, time.Date(2010, time.November, 7, 0, 0, 0, 0, time.UTC))

	var tests = []struct {
		year  int
		month time.Month
		day   int
		n     int
	}{
		{2010, time.November, 7, 2},
		{2010, time.November, 8, 0},
		{2012, time.January, 19, 1},
	}

	for _, tt := range tests {
		ttname := fmt.Sprintf("%d/%d/%d", tt.year, tt.month, tt.day)
		t.Run(ttname, func(t *testing.T) {
			tasks := getTasksByDueDate(t, tt.year, tt.month, tt.day)
			if len(tasks) != tt.n {
				t.Errorf("want %d books tasks, got %v", tt.n, tasks)
			}
		})
	}
}

func TestErrors(t *testing.T) {
	var tests = []struct {
		addr   string
		method string
	}{
		{"/foo/", "GET"},
		{"/foo/", "POST"},
		{"/task/", "PUT"},
		{"/task/11", "PUT"},
		{"/task/foobar", "GET"},
		{"/task/foobar", "DELETE"},

		{"/tag/todo", "POST"},
		{"/tag/todo", "DELETE"},

		{"/due/2020/11", "GET"},
		{"/due/2020/11/20", "POST"},
		{"/due/shell/11/20", "GET"},
		{"/due/2011/_/20", "GET"},
		{"/due/2020/11/x", "GET"},
	}

	for _, tt := range tests {
		ttname := fmt.Sprintf("%s->%s", tt.method, tt.addr)
		t.Run(ttname, func(t *testing.T) {
			req, err := http.NewRequest(tt.method, serverAddress()+tt.addr, nil)
			if err != nil {
				t.Fatal(err)
			}

			var client http.Client
			resp, err := client.Do(req)
			if err != nil {
				log.Fatal(err)
			}
			resp.Body.Close()

			if resp.StatusCode == http.StatusOK {
				t.Fatalf("want error code, got %v at %s->%s", resp.StatusCode, tt.method, tt.addr)
			}
		})
	}
}

package main

import (
	"encoding/json"
	"fmt"
	"log"
	"mime"
	"net/http"
	"os"
	"strconv"
	"sync"
	"time"

	"github.com/gorilla/mux"

	"example.com/internal/taskstore"
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

// renderJSON renders 'v' as JSON and writes it as a response into w.
func renderJSON(w http.ResponseWriter, v interface{}) {
	js, err := json.Marshal(v)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	w.Header().Set("Content-Type", "application/json")
	w.Write(js)
}

func (ts *taskServer) createTaskHandler(w http.ResponseWriter, req *http.Request) {
	log.Printf("handling task create at %s\n", req.URL.Path)

	// Types used internally in this handler to (de-)serialize the request and
	// response from/to JSON.
	type RequestTask struct {
		Text string    `json:"text"`
		Tags []string  `json:"tags"`
		Due  time.Time `json:"due"`
	}

	type ResponseId struct {
		Id int `json:"id"`
	}

	// Enforce a JSON Content-Type.
	contentType := req.Header.Get("Content-Type")
	mediatype, _, err := mime.ParseMediaType(contentType)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}
	if mediatype != "application/json" {
		http.Error(w, "expect application/json Content-Type", http.StatusUnsupportedMediaType)
		return
	}

	dec := json.NewDecoder(req.Body)
	dec.DisallowUnknownFields()
	var rt RequestTask
	if err := dec.Decode(&rt); err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	ts.Lock()
	id := ts.store.CreateTask(rt.Text, rt.Tags, rt.Due)
	defer ts.Unlock()

	renderJSON(w, ResponseId{Id: id})
}

func (ts *taskServer) getAllTasksHandler(w http.ResponseWriter, req *http.Request) {
	log.Printf("handling get all tasks at %s\n", req.URL.Path)

	ts.Lock()
	allTasks := ts.store.GetAllTasks()
	ts.Unlock()

	renderJSON(w, allTasks)
}

func (ts *taskServer) getTaskHandler(w http.ResponseWriter, req *http.Request) {
	log.Printf("handling get task at %s\n", req.URL.Path)

	// Here and elsewhere, not checking error of Atoi because the router only
	// matches the [0-9]+ regex.
	id, _ := strconv.Atoi(mux.Vars(req)["id"])
	ts.Lock()
	task, err := ts.store.GetTask(id)
	ts.Unlock()

	if err != nil {
		http.Error(w, err.Error(), http.StatusNotFound)
		return
	}

	renderJSON(w, task)
}

func (ts *taskServer) deleteTaskHandler(w http.ResponseWriter, req *http.Request) {
	log.Printf("handling delete task at %s\n", req.URL.Path)

	id, _ := strconv.Atoi(mux.Vars(req)["id"])
	ts.Lock()
	err := ts.store.DeleteTask(id)
	ts.Unlock()

	if err != nil {
		http.Error(w, err.Error(), http.StatusNotFound)
	}
}

func (ts *taskServer) deleteAllTasksHandler(w http.ResponseWriter, req *http.Request) {
	log.Printf("handling delete all tasks at %s\n", req.URL.Path)
	ts.Lock()
	ts.store.DeleteAllTasks()
	ts.Unlock()
}

func (ts *taskServer) tagHandler(w http.ResponseWriter, req *http.Request) {
	log.Printf("handling tasks by tag at %s\n", req.URL.Path)

	tag := mux.Vars(req)["tag"]
	ts.Lock()
	tasks := ts.store.GetTasksByTag(tag)
	ts.Unlock()

	renderJSON(w, tasks)
}

func (ts *taskServer) dueHandler(w http.ResponseWriter, req *http.Request) {
	log.Printf("handling tasks by tag at %s\n", req.URL.Path)

	vars := mux.Vars(req)
	badRequestError := func() {
		http.Error(w, fmt.Sprintf("expect /due/<year>/<month>/<day>, got %v", req.URL.Path), http.StatusBadRequest)
	}

	year, _ := strconv.Atoi(vars["year"])
	month, _ := strconv.Atoi(vars["month"])
	if month < int(time.January) || month > int(time.December) {
		badRequestError()
		return
	}
	day, _ := strconv.Atoi(vars["day"])

	ts.Lock()
	tasks := ts.store.GetTasksByDueDate(year, time.Month(month), day)
	ts.Unlock()

	renderJSON(w, tasks)
}

func main() {
	router := mux.NewRouter()
	router.StrictSlash(true)
	server := NewTaskServer()

	router.HandleFunc("/task/", server.createTaskHandler).Methods("POST")
	router.HandleFunc("/task/", server.getAllTasksHandler).Methods("GET")
	router.HandleFunc("/task/", server.deleteAllTasksHandler).Methods("DELETE")
	router.HandleFunc("/task/{id:[0-9]+}/", server.getTaskHandler).Methods("GET")
	router.HandleFunc("/task/{id:[0-9]+}/", server.deleteTaskHandler).Methods("DELETE")
	router.HandleFunc("/tag/{tag}/", server.tagHandler).Methods("GET")
	router.HandleFunc("/due/{year:[0-9]+}/{month:[0-9]+}/{day:[0-9]+}/", server.dueHandler).Methods("GET")

	log.Fatal(http.ListenAndServe("localhost:"+os.Getenv("SERVERPORT"), router))
}

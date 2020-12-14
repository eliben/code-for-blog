package main

import (
	"encoding/json"
	"fmt"
	"log"
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

func (ts *taskServer) dueHandler(w http.ResponseWriter, req *http.Request) {
	log.Printf("handling tasks by tag at %s\n", req.URL.Path)

	vars := mux.Vars(req)
	badRequestError := func() {
		http.Error(w, fmt.Sprintf("expect /due/<year>/<month>/<day>, got %v", req.URL.Path), http.StatusBadRequest)
	}

	year, err := strconv.Atoi(vars["year"])
	if err != nil {
		badRequestError()
		return
	}
	month, err := strconv.Atoi(vars["month"])
	if err != nil || month < int(time.January) || month > int(time.December) {
		badRequestError()
		return
	}
	day, err := strconv.Atoi(vars["day"])
	if err != nil {
		badRequestError()
		return
	}

	ts.Lock()
	tasks := ts.store.GetTasksByDueDate(year, time.Month(month), day)
	ts.Unlock()

	renderJSON(w, tasks)
}

func main() {
	router := mux.NewRouter()
	router.StrictSlash(true)

	server := NewTaskServer()

	router.HandleFunc(
		"/due/{year:[0-9]+}/{month:[0-9]+}/{day:[0-9]+}",
		server.dueHandler).Methods("GET")

	//server := NewTaskServer()
	//mux.HandleFunc("/task/", server.taskHandler)
	//mux.HandleFunc("/tag/", server.tagHandler)
	//mux.HandleFunc("/due/", server.dueHandler)

	log.Fatal(http.ListenAndServe("localhost:"+os.Getenv("SERVERPORT"), router))
}

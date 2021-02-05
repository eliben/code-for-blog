// REST API implementation for the task server.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package swagger

import (
	"encoding/json"
	"fmt"
	"mime"
	"net/http"
	"strconv"
	"time"

	"example.com/internal/taskstore"
	"github.com/gorilla/mux"
)

// This is global because the generated server routes point to top-level
// functions
var store = taskstore.New()

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

func DueYearMonthDayGet(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	badRequestError := func() {
		http.Error(w, fmt.Sprintf("expect /due/<year>/<month>/<day>, got %v", r.URL.Path), http.StatusBadRequest)
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

	tasks := store.GetTasksByDueDate(year, time.Month(month), day)
	renderJSON(w, tasks)
}

func TagTagnameGet(w http.ResponseWriter, r *http.Request) {
	tag := mux.Vars(r)["tagname"]
	tasks := store.GetTasksByTag(tag)
	renderJSON(w, tasks)
}

func TaskGet(w http.ResponseWriter, r *http.Request) {
	allTasks := store.GetAllTasks()
	renderJSON(w, allTasks)
}

func TaskDeleteAll(w http.ResponseWriter, r *http.Request) {
	store.DeleteAllTasks()
}

func TaskIdDelete(w http.ResponseWriter, r *http.Request) {
	id, err := strconv.Atoi(mux.Vars(r)["id"])
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}
	err = store.DeleteTask(id)
	if err != nil {
		http.Error(w, err.Error(), http.StatusNotFound)
	}
}

func TaskIdGet(w http.ResponseWriter, r *http.Request) {
	id, err := strconv.Atoi(mux.Vars(r)["id"])
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}
	task, err := store.GetTask(id)
	if err != nil {
		http.Error(w, err.Error(), http.StatusNotFound)
		return
	}

	renderJSON(w, task)
}

func TaskPost(w http.ResponseWriter, r *http.Request) {
	type ResponseId struct {
		Id int `json:"id"`
	}

	// Enforce a JSON Content-Type.
	contentType := r.Header.Get("Content-Type")
	mediatype, _, err := mime.ParseMediaType(contentType)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}
	if mediatype != "application/json" {
		http.Error(w, "expect application/json Content-Type", http.StatusUnsupportedMediaType)
		return
	}

	dec := json.NewDecoder(r.Body)
	dec.DisallowUnknownFields()
	var rt Body
	if err := dec.Decode(&rt); err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	id := store.CreateTask(rt.Text, rt.Tags, rt.Due)
	renderJSON(w, ResponseId{Id: id})
}

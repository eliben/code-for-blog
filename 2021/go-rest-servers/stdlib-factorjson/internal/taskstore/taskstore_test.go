package taskstore

import (
	"testing"
	"time"
)

func TestCreateAndGet(t *testing.T) {
	// Create a store and a single task.
	ts := New()
	id := ts.CreateTask("Hola", nil, time.Now())

	// We should be able to retrieve this task by ID, but nothing with other
	// IDs.
	task, err := ts.GetTask(id)
	if err != nil {
		t.Fatal(err)
	}

	if task.Id != id {
		t.Errorf("got task.Id=%d, id=%d", task.Id, id)
	}
	if task.Text != "Hola" {
		t.Errorf("got Text=%v, want %v", task.Text, "Hola")
	}

	// Asking for all tasks, we only get the one we put in.
	allTasks := ts.GetAllTasks()
	if len(allTasks) != 1 || allTasks[0].Id != id {
		t.Errorf("got len(allTasks)=%d, allTasks[0].Id=%d; want 1, %d", len(allTasks), allTasks[0].Id, id)
	}

	_, err = ts.GetTask(id + 1)
	if err == nil {
		t.Fatal("got nil, want error")
	}

	// Add another task. Expect to find two tasks in the store.
	ts.CreateTask("hey", nil, time.Now())
	allTasks2 := ts.GetAllTasks()
	if len(allTasks2) != 2 {
		t.Errorf("got len(allTasks2)=%d; want 2", len(allTasks2))
	}
}

func TestDelete(t *testing.T) {
	ts := New()
	id1 := ts.CreateTask("Foo", nil, time.Now())
	id2 := ts.CreateTask("Bar", nil, time.Now())

	if err := ts.DeleteTask(id1 + 1001); err == nil {
		t.Fatalf("delete task id=%d, got no error; want error", id1+1001)
	}

	if err := ts.DeleteTask(id1); err != nil {
		t.Fatal(err)
	}
	if err := ts.DeleteTask(id1); err == nil {
		t.Fatalf("delete task id=%d, got no error; want error", id1)
	}

	if err := ts.DeleteTask(id2); err != nil {
		t.Fatal(err)
	}
}

func TestDeleteAll(t *testing.T) {
	ts := New()
	ts.CreateTask("Foo", nil, time.Now())
	ts.CreateTask("Bar", nil, time.Now())

	if err := ts.DeleteAllTasks(); err != nil {
		t.Fatal(err)
	}

	tasks := ts.GetAllTasks()
	if len(tasks) > 0 {
		t.Fatalf("want no tasks remaining; got %v", tasks)
	}
}

func TestGetTasksByTag(t *testing.T) {
	ts := New()
	ts.CreateTask("XY", []string{"Movies"}, time.Now())
	ts.CreateTask("YZ", []string{"Bills"}, time.Now())
	ts.CreateTask("YZR", []string{"Bills"}, time.Now())
	ts.CreateTask("YWZ", []string{"Bills", "Movies"}, time.Now())
	ts.CreateTask("WZT", []string{"Movies", "Bills"}, time.Now())

	var tests = []struct {
		tag     string
		wantNum int
	}{
		{"Movies", 3},
		{"Bills", 4},
		{"Ferrets", 0},
	}

	for _, tt := range tests {
		t.Run(tt.tag, func(t *testing.T) {
			numByTag := len(ts.GetTasksByTag(tt.tag))

			if numByTag != tt.wantNum {
				t.Errorf("got %v, want %v", numByTag, tt.wantNum)
			}
		})
	}
}

func TestGetTasksByDueDate(t *testing.T) {
	timeFormat := "2006-Jan-02"
	mustParseDate := func(tstr string) time.Time {
		tt, err := time.Parse(timeFormat, tstr)
		if err != nil {
			t.Fatal(err)
		}
		return tt
	}

	ts := New()
	ts.CreateTask("XY1", nil, mustParseDate("2020-Dec-01"))
	ts.CreateTask("XY2", nil, mustParseDate("2000-Dec-21"))
	ts.CreateTask("XY3", nil, mustParseDate("2020-Dec-01"))
	ts.CreateTask("XY4", nil, mustParseDate("2000-Dec-21"))
	ts.CreateTask("XY5", nil, mustParseDate("1991-Jan-01"))

	// Check a single task can be fetched.
	y, m, d := mustParseDate("1991-Jan-01").Date()
	tasks1 := ts.GetTasksByDueDate(y, m, d)
	if len(tasks1) != 1 {
		t.Errorf("got len=%d, want 1", len(tasks1))
	}
	if tasks1[0].Text != "XY5" {
		t.Errorf("got Text=%s, want XY5", tasks1[0].Text)
	}

	var tests = []struct {
		date    string
		wantNum int
	}{
		{"2020-Jan-01", 0},
		{"2020-Dec-01", 2},
		{"2000-Dec-21", 2},
		{"1991-Jan-01", 1},
		{"2020-Dec-21", 0},
	}

	for _, tt := range tests {
		t.Run(tt.date, func(t *testing.T) {
			y, m, d := mustParseDate(tt.date).Date()
			numByDate := len(ts.GetTasksByDueDate(y, m, d))

			if numByDate != tt.wantNum {
				t.Errorf("got %v, want %v", numByDate, tt.wantNum)
			}
		})
	}
}

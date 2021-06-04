// package taskstore provides a simple in-memory "data store" for tasks.
// Tasks are uniquely identified by numeric IDs.
//
// This package was modified from the earlier parts in the REST series to use
// graph/model.Task and its subtypes and add attachments; each Task here has a
// slice of Attachment.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package taskstore

import (
	"fmt"
	"sync"
	"time"

	"example.com/graph/model"
)

// TaskStore is a simple in-memory database of tasks; TaskStore methods are
// safe to call concurrently.
type TaskStore struct {
	sync.Mutex

	tasks  map[int]*model.Task
	nextId int
}

func New() *TaskStore {
	ts := &TaskStore{}
	ts.tasks = make(map[int]*model.Task)
	ts.nextId = 0
	return ts
}

// CreateTask creates a new task in the store.
func (ts *TaskStore) CreateTask(text string, tags []string, due time.Time, attachments []*model.Attachment) int {
	ts.Lock()
	defer ts.Unlock()

	task := &model.Task{
		ID:          ts.nextId,
		Text:        text,
		Due:         due,
		Attachments: attachments}
	task.Tags = make([]string, len(tags))
	copy(task.Tags, tags)

	ts.tasks[ts.nextId] = task
	ts.nextId++
	return task.ID
}

// GetTask retrieves a task from the store, by id. If no such id exists, an
// error is returned.
func (ts *TaskStore) GetTask(id int) (*model.Task, error) {
	ts.Lock()
	defer ts.Unlock()

	t, ok := ts.tasks[id]
	if ok {
		return t, nil
	} else {
		return nil, fmt.Errorf("task with id=%d not found", id)
	}
}

// DeleteTask deletes the task with the given id. If no such id exists, an error
// is returned.
func (ts *TaskStore) DeleteTask(id int) error {
	ts.Lock()
	defer ts.Unlock()

	if _, ok := ts.tasks[id]; !ok {
		return fmt.Errorf("task with id=%d not found", id)
	}

	delete(ts.tasks, id)
	return nil
}

// DeleteAllTasks deletes all tasks in the store.
func (ts *TaskStore) DeleteAllTasks() error {
	ts.Lock()
	defer ts.Unlock()

	ts.tasks = make(map[int]*model.Task)
	return nil
}

// GetAllTasks returns all the tasks in the store, in arbitrary order.
func (ts *TaskStore) GetAllTasks() []*model.Task {
	ts.Lock()
	defer ts.Unlock()

	allTasks := make([]*model.Task, 0, len(ts.tasks))
	for _, task := range ts.tasks {
		allTasks = append(allTasks, task)
	}
	return allTasks
}

// GetTasksByTag returns all the tasks that have the given tag, in arbitrary
// order.
func (ts *TaskStore) GetTasksByTag(tag string) []*model.Task {
	ts.Lock()
	defer ts.Unlock()

	var tasks []*model.Task

taskloop:
	for _, task := range ts.tasks {
		for _, taskTag := range task.Tags {
			if taskTag == tag {
				tasks = append(tasks, task)
				continue taskloop
			}
		}
	}
	return tasks
}

// GetTasksByDueDate returns all the tasks that have the given due date, in
// arbitrary order.
func (ts *TaskStore) GetTasksByDueDate(year int, month time.Month, day int) []*model.Task {
	ts.Lock()
	defer ts.Unlock()

	var tasks []*model.Task

	for _, task := range ts.tasks {
		y, m, d := task.Due.Date()
		if y == year && m == month && d == day {
			tasks = append(tasks, task)
		}
	}

	return tasks
}

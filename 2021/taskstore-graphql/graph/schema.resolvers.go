package graph

// This file will be automatically regenerated based on the schema, any resolver implementations
// will be copied through when generating and any unknown code will be moved to the end.

import (
	"context"
	"time"

	"example.com/graph/generated"
	"example.com/graph/model"
	"example.com/internal/taskstore"
)

func (r *mutationResolver) CreateTask(ctx context.Context, input model.NewTask) (*model.Task, error) {
	id := r.Store.CreateTask(input.Text, input.Tags, input.Due)
	task, err := r.Store.GetTask(id)
	if err != nil {
		return nil, err
	} else {
		mtask := model.Task(task)
		return &mtask, err
	}
}

func (r *mutationResolver) DeleteTask(ctx context.Context, id int) (*bool, error) {
	return nil, r.Store.DeleteTask(id)
}

func (r *mutationResolver) DeleteAllTasks(ctx context.Context) (*bool, error) {
	return nil, r.Store.DeleteAllTasks()
}

// convertTaskSlice converts a slice of taskstore.Task to a slice of *model.Task
// (as required by the GraphQL resolvers).
func convertTaskSlice(tasks []taskstore.Task) []*model.Task {
	mtasks := make([]*model.Task, 0, len(tasks))
	for _, t := range tasks {
		mtask := model.Task(t)
		mtasks = append(mtasks, &mtask)
	}
	return mtasks
}

func (r *queryResolver) GetAllTasks(ctx context.Context) ([]*model.Task, error) {
	tasks := r.Store.GetAllTasks()
	return convertTaskSlice(tasks), nil
}

func (r *queryResolver) GetTask(ctx context.Context, id int) (*model.Task, error) {
	task, err := r.Store.GetTask(id)
	if err != nil {
		return nil, err
	} else {
		mtask := model.Task(task)
		return &mtask, nil
	}
}

func (r *queryResolver) GetTasksByTag(ctx context.Context, tag string) ([]*model.Task, error) {
	tasks := r.Store.GetTasksByTag(tag)
	return convertTaskSlice(tasks), nil
}

func (r *queryResolver) GetTasksByDue(ctx context.Context, due time.Time) ([]*model.Task, error) {
	y, m, d := due.Date()
	tasks := r.Store.GetTasksByDueDate(y, m, d)
	return convertTaskSlice(tasks), nil
}

// Mutation returns generated.MutationResolver implementation.
func (r *Resolver) Mutation() generated.MutationResolver { return &mutationResolver{r} }

// Query returns generated.QueryResolver implementation.
func (r *Resolver) Query() generated.QueryResolver { return &queryResolver{r} }

type mutationResolver struct{ *Resolver }
type queryResolver struct{ *Resolver }

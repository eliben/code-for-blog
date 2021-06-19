package graph

// This file will be automatically regenerated based on the schema, any resolver implementations
// will be copied through when generating and any unknown code will be moved to the end.

import (
	"context"
	"time"

	"example.com/graph/generated"
	"example.com/graph/model"
)

func (r *mutationResolver) CreateTask(ctx context.Context, input model.NewTask) (*model.Task, error) {
	attachments := make([]*model.Attachment, 0, len(input.Attachments))
	for _, a := range input.Attachments {
		attachments = append(attachments, (*model.Attachment)(a))
	}
	id := r.Store.CreateTask(input.Text, input.Tags, input.Due, attachments)
	task, err := r.Store.GetTask(id)
	return task, err
}

func (r *mutationResolver) DeleteTask(ctx context.Context, id int) (*bool, error) {
	return nil, r.Store.DeleteTask(id)
}

func (r *mutationResolver) DeleteAllTasks(ctx context.Context) (*bool, error) {
	return nil, r.Store.DeleteAllTasks()
}

func (r *queryResolver) GetAllTasks(ctx context.Context) ([]*model.Task, error) {
	return r.Store.GetAllTasks(), nil
}

func (r *queryResolver) GetTask(ctx context.Context, id int) (*model.Task, error) {
	return r.Store.GetTask(id)
}

func (r *queryResolver) GetTasksByTag(ctx context.Context, tag string) ([]*model.Task, error) {
	return r.Store.GetTasksByTag(tag), nil
}

func (r *queryResolver) GetTasksByDue(ctx context.Context, due time.Time) ([]*model.Task, error) {
	y, m, d := due.Date()
	return r.Store.GetTasksByDueDate(y, m, d), nil
}

// Mutation returns generated.MutationResolver implementation.
func (r *Resolver) Mutation() generated.MutationResolver { return &mutationResolver{r} }

// Query returns generated.QueryResolver implementation.
func (r *Resolver) Query() generated.QueryResolver { return &queryResolver{r} }

type mutationResolver struct{ *Resolver }
type queryResolver struct{ *Resolver }
